\m4_TLV_version 1d: tl-x.org
\m5
  // Instruction field values for each instruction are defined as localparams and as M4 defines. Assembly uses one or the other
  // depending on m5_use_localparams.
  // Define localparam and m5_ constant.
  // m5_define_localparam(<name>, <localparam-bit-range>, <value>)
  default_def(use_localparams, 0)
  def(define_localparam,
      ['m4_define(['m5_$1'], ['$3'])m4_ifelse(m5_use_localparams, 1, ['['localparam $2 $1 = $3;']'])'])
  // Use defined localparam or m5_ constant, depending on m5_use_localparams.
  def(localparam_value,
      ['m4_ifelse(m5_use_localparams, 1, [''], ['m5_'])$1'])

  // --------------------------------------
  // Associate each op5 value with an instruction type.
  // --------------------------------------

  // TODO:
  // We construct m5_OP5_XXXXX_TYPE, and verify each instruction against that.
  // Instruction fields are constructed and valid based on op5.
  //...
  // TO UPDATE:
  // We construct localparam INSTR_TYPE_X_MASK as a mask, one bit per op5 indicating whether the op5 is of the type.
  // Instantiated recursively for each instruction type.
  // Initializes m5_instr_type_X_mask_expr which will build up a mask, one bit per op5.
  def(instr_types,
      ['m4_ifelse(['$1'], [''], [''],
                  ['m4_define(['m5_instr_type_$1_mask_expr'], ['0'])m5_instr_types(m4_shift($@))'])'])
  // Instantiated recursively for each instruction type in \SV_plus context after characterizing each type.
  // Declares localparam INSTR_TYPE_X_MASK as m5_instr_type_X_mask_expr.
  fn(instr_types_sv, ..., [
     ~ifeq(['$1'], [''], [''], [
        ~(['    ']m5_define_localparam(['INSTR_TYPE_$1_MASK'], ['[31:0]'], m5_instr_type_$1_mask_expr)m5_nl)
        ~instr_types_sv(m4_shift($@))
     ])
  ])
  // Instantiated recursively for each instruction type in \SV_plus context to decode instruction type.
  // Creates "assign $$is_x_type = INSTR_TYPE_X_MASK[$raw_op5];" for each type.
  // TODO: Not sure how to extract a bit ($raw_op) from a constant expression. Hoping synthesis optimizes well.
  def(types_decode,
         ['m4_ifelse(['$1'], [''], [''],
                     ['m4_nl['   assign $$is_']m4_translit(['$1'], ['A-Z'], ['a-z'])['_type = (((']m5_localparam_value(['INSTR_TYPE_$1_MASK'])[') >> $raw_op5) & 32'b1) != 32'b0; ']m5_types_decode(m4_shift($@))'])'])
  // Instantiated for each op5 in \SV_plus context.
  fn(op5, ..., [
     def(OP5_$1_TYPE, $2)
     var(op5_named_$2, $1)
     ~nl(['   ']m5_define_localparam(['OP5_$3'], ['[4:0]'], ['5'b$1']))
     def(instr_type_$2_mask_expr, m4_quote(m5_instr_type_$2_mask_expr)[' | (1 << 5'b$1)'])
  ])


  // --------------------------------
  // Characterize each instruction mnemonic
  // --------------------------------

  // Each instruction is defined by instantiating m5_instr(...), e.g.: 
  //    m5_instr(B, 32, I, 11000, 000, BEQ)
  // which instantiates an instruction-type-specific macro, e.g.:
  //    m4_instrB(32, I, 11000, 000, BEQ)
  // which produces (or defines macros for):
  //   o instruction decode logic ($is_<mnemonic>_instr = ...;)
  //   o for debug, an expression to produce the MNEMONIC.
  //   o result MUX expression to select result of the appropriate execution expression
  //   o $illegal_instruction expression
  //   o localparam definitions for fields
  //   o m5_asm(<MNEMONIC>, ...) to assemble instructions to their binary representations

  // Return 1 if the given instruction is supported, [''] otherwise.
  // m5_instr_supported(<args-of-m5_instr(...)>)
  def(instr_supported,
      ['m4_ifelse(m5_EXT_$3, 1,
                  ['m4_ifelse(m5_WORD_CNT, ['$2'], 1, [''])'])'])

  // Called for each instruction.
  // Outputs a string to evaluate that outputs indented content for \SV_plus context.
  // m5_instr(<instr-type-char(s)>, <type-specific-args>)
  // This instantiates m5_instr<type>(type-specific-args>)
  fn(instr, [1]type, [2]width, [3]ext, [4]op5, ..., [
     var(mnemonic, m5_argn($#, $@))
     // check instr type
     ifne(m5_OP5_$4_TYPE, m5_ifdef(['instr_type_of_$1'], m5_instr_type_of_$1, ['$1']),
        ['m5_error(['Instruction ']m5_mnemonic[''s type (']m5_type[') is inconsistant with its op5 code (']m5_op5[') of type ']m5_eval(['m5_OP5_']m5_op5['_TYPE'])['.'])'])
     // if instrs extension is supported and instr is for the right machine width, include it
     ~ifeq(m5_instr_supported($@), 1, [
        ~(['   ']m5_define_localparam(m5_mnemonic['_INSTR_OPCODE'], ['[6:0]'], ['7'b']m5_op5['11']))
        ~(m4_instr$1(m5_mnemonic, m5_shift($@))m5_nl)
     ])
  ])


  // Decode logic for instructions with various opcode/func bits that dictate the mnemonic.
  // (This would be easier if we could use 'x', but Yosys doesn't support ==?/!=? operators.)
  // Helpers to deal with "rm" cases:
  macro(op5_and_funct3,
     ['$raw_op5 == 5'b$3 m4_ifelse($4, ['rm'], [''], ['&& $raw_funct3 == 3'b$4'])'])
  macro(funct3_localparam,
     ['m4_ifelse(['$2'], ['rm'], [''], ['m5_define_localparam(['$1_INSTR_FUNCT3'], ['[2:0]'], ['3'b$2'])'])'])
  // m4_asm_<MNEMONIC> output for funct3 or rm, returned in unquoted context so arg references can be produced. 'rm' is always the last m4_asm_<MNEMONIC> arg (m4_arg(#)).
  //   Args: $1: MNEMONIC, $2: funct3 field of instruction definition (or 'rm')
  // TODO: Remove "new_" from name below.
  macro(asm_funct3, ['['m4_ifelse($2, ['rm'], ['3'b']m5_rm, m5_localparam_value(['$1_INSTR_FUNCT3']))']'])
  
  // Opcode + funct3 + funct7 (R-type, R2-type). $@ as for m4_instrX(..), $7: MNEMONIC, $8: number of bits of leading bits of funct7 to interpret. If 5, for example, use the term funct5, $9: (opt) for R2, the r2 value.
  macro(instr_funct7,
     ['m5_instr_decode_expr($7, m5_op5_and_funct3($@)[' && $raw_funct7'][6:m4_eval(7-$8)][' == $8'b$5']m4_ifelse($9, [''], [''], [' && $raw_rs2 == 5'b$9']))m5_funct3_localparam(['$7'], ['$4'])m5_define_localparam(['$7_INSTR_FUNCT$8'], ['[$8-1:0]'], ['$8'b$5'])'])
  // For cases w/ extra shamt bit that cuts into funct7.
  macro(instr_funct6,
     ['m5_instr_decode_expr($7, m5_op5_and_funct3($@)[' && $raw_funct7[6:1] == 6'b$5'])m5_funct3_localparam(['$7'], ['$4'])m5_define_localparam(['$7_INSTR_FUNCT6'], ['[6:0]'], ['6'b$5'])'])
  // Opcode + funct3 + func7[1:0] (R4-type)
  macro(instr_funct2,
         ['m5_instr_decode_expr($6, m5_op5_and_funct3($@)[' && $raw_funct7[1:0] == 2'b$5'])m5_funct3_localparam(['$6'], ['$4'])m5_define_localparam(['$6_INSTR_FUNCT2'], ['[1:0]'], ['2'b$5'])'])
  // Opcode + funct3 + funct7[6:2] (R-type where funct7 has two lower bits that do not distinguish mnemonic.)
  macro(instr_funct5,
         ['m5_instr_decode_expr($6, m5_op5_and_funct3($@)[' && $raw_funct7[6:2] == 5'b$5'])m5_funct3_localparam(['$6'], ['$4'])m5_define_localparam(['$6_INSTR_FUNCT5'], ['[4:0]'], ['5'b$5'])'])
  // Opcode + funct3
  macro(instr_funct3,
         ['m5_instr_decode_expr($5, m5_op5_and_funct3($@), $6)m5_funct3_localparam(['$5'], ['$4'])'])
  // Opcode
  fn(instr_no_func, mnemonic, op5, [
     instr_decode_expr(m5_mnemonic, ['$raw_op5 == 5'b']m5_op5)
  ])

  macro(instr_viz,
     ['m4_ifelse(['$1'], [''], [''],
                 ['['is_instr("$1", '$is_']['$1']['_instr'); ']$0(m4_shift($@))'])'])
  // m5_instr_decode_expr macro
  // Args: (MNEMONIC, decode_expr, (opt)['no_dest']/other)
  // Extends the following definitions to reflect the given instruction <mnemonic>:
  def(['# instructiton decode: $is_<mnemonic>_instr = ...; ...'],
      decode_expr, [''],
      ['# result combining expr.: ({32{$is_<mnemonic>_instr}} & $<mnemonic>_rslt) | ...'],
      rslt_mux_expr, [''],
      ['# $illegal instruction exception expr: && ! $is_<mnemonic>_instr ...'],
      illegal_instr_expr, [''],
      ['# $is_<mnemonic>_instr ? "<MNEMONIC>" : ...'],
      mnemonic_expr, [''])
  fn(instr_decode_expr, mnemonic, expr, ..., {
     // Lower case instruction name
     var(lc_name, m4_translit(m5_mnemonic, ['A-Z'], ['a-z']))
     append_macro(decode_expr, ['$is_']m5_lc_name['_instr = ']m5_expr[';']m4_nl)
     ifeq(['$1'], ['no_dest'],
        [''],
        ['m5_append_macro(rslt_mux_expr, [' |']['m4_plus_new_line       ({']m5_WORD_CNT['{$is_']m5_lc_name['_instr}} & $']m5_lc_name['_rslt)'])'])
     append_macro(illegal_instr_expr, [' && ! $is_']m5_lc_name['_instr'])
     append_macro(mnemonic_expr, ['$is_']m5_lc_name['_instr ? "']m5_mnemonic['']m4_substr(['          '], m4_len(m5_mnemonic))['" : '])
  })

  // The first arg of m5_instr(..) is a type, and a type-specific macro is invoked. Types are those defined by RISC-V, plus:
  //   R2: R-type with a hard-coded rs2 value. (assuming illegal instruction exception should be thrown for wrong value--not clear in RISC-V spec)
  //   If: I-type with leading bits of imm[11:...] used as function bits.

  def(instr_type_of_R2, R,
      instr_type_of_If, I)
  // Unique to each instruction type, eg:
  //   ~instr(U, 32, I, 01101,      LUI)
  //   ~instr(J, 32, I, 11011,      JAL)
  //   ~instr(B, 32, I, 11000, 000, BEQ)
  //   ~instr(S, 32, I, 01000, 000, SB)
  //   ~instr(I, 32, I, 00100, 000, ADDI)
  //   ~instr(If, 64, I, 00100, 101, 000000, SRLI)  // (imm[11:6] are used like funct7[6:1] and must be 000000)
  //   ~instr(R, 32, I, 01100, 000, 0000000, ADD)
  //   ~instr(R4, 32, F, 10000, rm, 10, FMADD.D)
  //   ~instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRT.S)
  //   ~instr(R2, 32, A, 01011, 010, 00010, 00000, LR.W)  // (5 bits for funct7 for all "A"-ext instrs)
  //   ~instr(R, 32, A, 01011, 010, 00011, SC.W)          //   "
  // This defines assembler macros as follows. Fields are ordered rd, rs1, rs2, imm:
  //   I:  m5_asm_ADDI(r4, r1, 0),
  //   R:  m5_asm_ADD(r4, r1, r2),
  //   R2: m5_asm_FSQRT.S(r4, r1, 000),  // rm == 000
  //   R4: m5_asm_FMADD.S(r4, r1, r2, r3, 000),  // rm == 000
  //   S:  m5_asm_SW(r1, r2, 100),  // Store r13 into [r10] + 4
  //   J:  m5_asm_JAL(r7, 000000000000000000010),  // Jump to next instr. Up to 21-bit signed immediate including 0 representing implicit bit-0 (unlike JALR) provides byte address
  //   B:  m5_asm_BLT(r1, r2, 1000), // Branch if r1 < r2 to PC + 13'b1000 (where lsb = 0)
  //   For "A"-extension instructions, an additional final arg is REQUIRED to provide 2 binary bits for aq and rl.
  // Macro definitions include 2 parts:
  //   o Hardware definitions: m5_instr_<mnemonic>($@)
  //   o Assembler definition of m5_asm_<MNEMONIC>: m4_define(['m5_asm_<MNEMONIC>'], ['m5_asm_instr_str(...)'])
  fn(instrI, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^mnemonic, {
        asm_instr_str(I, m5_mnemonic, m5_fn_args)
        ~(['{12'b']m5_imm[', ']m5_asm_reg(m5_src1)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT3'])[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['})
     })
  })
  fn(instrIf, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct7($@, m5_mnemonic, m4_len(m5_imm_funct))
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^imm_funct, ^mnemonic, {
        asm_instr_str(I, m5_mnemonic, m5_fn_args)
        ~(['{']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT']m4_len(m5_imm_funct))[', ']m4_eval(12-m4_len(m5_imm_funct))[''b']m5_imm[', ']m5_asm_reg(m5_src1)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT3'])[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['})
     })
  })
  fn(instrR, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct7($@, m5_mnemonic, m4_ifelse(m5_ext, ['A'], 5, 7))
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]src2, ?[4]rm, ^ext, ^funct3, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m5_fn_args)
        ~(['{']m4_ifelse(m5_ext, ['A'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT5'])[', ']m5_src1'], m5_localparam_value(m5_mnemonic['_INSTR_FUNCT7']))[', ']m5_asm_reg(m5_src2)[', ']m5_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrR2, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, [6]fixed_src2, ..., {
     ~instr_funct7($@, 7, m5_fixed_src2)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, ?[3]rm, ^ext, ^funct3, ^fixed_src2, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m5_fn_args)
        ~(['{']m4_ifelse(m5_ext, ['A'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT5'])[', ']m5_src1'], m5_localparam_value(m5_mnemonic['_INSTR_FUNCT7']))[', 5'b']m5_fixed_src2[', ']m5_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrR4, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct2($@)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]src2, [4]src3, ?[5]rm, ^funct3, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m5_fn_args)
        ~(['{']m5_asm_reg(m5_src3)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT2'])[', ']m5_asm_reg(m5_src2)[', ']m5_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrS, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@, ['no_dest'])
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^mnemonic, {
        asm_instr_str(S, m5_mnemonic, m5_fn_args)
        ~(['{']m5_asm_imm_field(m5_imm, 12, 11, 5)[', ']m5_asm_reg(m5_src1)[', ']m5_asm_reg(m5_dest)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m5_asm_imm_field(m5_imm, 12, 4, 0)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrB, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@, no_dest)
     fn(['asm_']m5_mnemonic, [1]src1, [2]src2, [3]target, ^funct3, ^mnemonic, {
        def(imm, m5_asm_target(13))
        asm_instr_str(B, m5_mnemonic, m5_fn_args)
        ~({m5_asm_imm_field(m5_imm, 13, 12, 12)[', ']m5_asm_imm_field(m5_imm, 13, 10, 5)[', ']m5_asm_reg(m5_src2)[', ']m5_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m5_asm_imm_field(m5_imm, 13, 4, 1)[', ']m5_asm_imm_field(m5_imm, 13, 11, 11)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['})
     })
  })
  fn(instrU, mnemonic, [1]width, [2]ext, [3]op5, ..., {
     ~instr_no_func(m5_mnemonic, m5_op5)
     fn(['asm_']m5_mnemonic, [1]dest, [2]imm, ^mnemonic, {
        asm_instr_str(U, m5_mnemonic, m5_fn_args)
        ~(['{']m5_asm_imm_field(m5_imm, 20, 19, 0)[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrJ, mnemonic, [1]width, [2]ext, [3]op5, ..., {
     ~instr_no_func(m5_mnemonic, m5_op5)
     fn(['asm_']m5_mnemonic, [1]dest, [2]target, ^mnemonic, {
        def(imm, m5_asm_target(21))
        asm_instr_str(J, m5_mnemonic, m5_fn_args)
        ~(['{']m5_asm_imm_field(m5_imm, 21, 20, 20)[', ']m5_asm_imm_field(m5_imm, 21, 10, 1)[', ']m5_asm_imm_field(m5_imm, 21, 11, 11)[', ']m5_asm_imm_field(m5_imm, 21, 19, 12)[', ']m5_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instr_, mnemonic, [1]width, [2]ext, [3]op5, {
     ~instr_no_func(m5_mnemonic, m5_op5)
  })

  // For each instruction type.
  // Declare localparam[31:0] INSTR_TYPE_X_MASK, initialized to 0 that will be given a 1 bit for each op5 value of its type.
  def(instr_types_args, ['I, R, R2, R4, S, B, J, U, _'])
  instr_types(m5_instr_types_args)


  // Instruction fields (User ISA Manual 2.2, Fig. 2.2)
  // TODO: m5_define_fields doesn't exist, but this didn't break WARP-V?
  define_fields(INSTR, 32, FUNCT7, 25, RS2, 20, RS1, 15, FUNCT3, 12, RD, 7, OP5, 2, OP2, 0)

  //=========
  // Specifically for assembler.

  // For labels:
  // m4_[un]signed_int_to_fixed_binary(digits, value)
  // Returns a string of 0s and 1s, representing the zero-padded binary value.
  // Args:
  //   digits: number of binary digits
  //   value: value (for signed, must be in the range -2**digits .. 2**digits-1,
  //                 for unsigned, must be in the range 0 .. 2**digits)
  def(unsigned_int_to_fixed_binary, ['m4_ifelse(m4_eval($1 > 1), 1, ['m5_unsigned_int_to_fixed_binary(m4_eval($1-1), m4_eval($2 >> 1))'])['']m4_eval($2 % 2)'])
  def(signed_int_to_fixed_binary, ['m5_unsigned_int_to_fixed_binary(['$1'], m4_ifelse(m4_eval($2 >= 0), 1, ['$2'], ['m4_eval($2 + 2 ** $1)']))'])
  def(label, ['m4_def(label_$1_addr, m5_NUM_INSTRS)'])
  =m4_def(label, m5_defn(['m5_label']))
  // m5_label_to_imm(label, bit-width): Convert a label (excluding :) to an immediate for current m5_NUM_INSTRS.
  def(label_to_imm, ['m5_signed_int_to_fixed_binary($2, m4_ifdef(['m4_label_$1_addr'], ['m4_eval((m4_label_$1_addr - m5_NUM_INSTRS) * 4)'], ['m4_errprint(['No assembler label "']$1['"']m4_nl)0000000000000']))'])
  
  // m5_asm_target(width): Output the offset for a given branch target arg in m4_target of the form :label or 1111111111000, with the given bit width.
  def(asm_target, ['m4_ifelse(m5_extract_prefix_eval([':'], target), [':'], ['m5_label_to_imm(m5_target, $1)'], ['m5_target'])'])


  // An 20-bit immediate binary zero string.
  def(asm_imm_zero, ['00000000000000000000'])
  // Zero-extend to n bits. E.g. m5_asm_zero_ext(1001, 7) => 0001001
  def(asm_zero_ext, ['m4_substr(m5_asm_imm_zero, 0, m4_eval($2 - m4_len($1)))$1'])
  // Extract bits from a binary immediate value.
  // m5_asm_imm_field(binary-imm, imm-length, max-bit, min-bit)
  // E.g. m5_asm_imm_field(101011, 17, 7, 3) => 5'b00101
  def(asm_imm_field, ['m4_eval($3 - $4 + 1)'b['']m4_substr(m5_asm_zero_ext($1, $2), m4_eval($2 - $3 - 1), m4_eval($3 - $4 + 1))'])
  // Register operand.
  def(asm_reg, ['m4_ifelse(m4_substr(['$1'], 0, 1), ['r'], [''], ['m4_ifelse(m4_substr(['$1'], 0, 1), ['x'], [''], ['m4_errprint(['$1 passed to register field.'])'])'])5'd['']m4_substr(['$1'], 1)'])

  // For debug, a string for an asm instruction.
  def(asm_instr_str, ['m4_with(str, ['['($1) $2 ']']m4_dquote(m4_shift(m4_shift($@))),
                                  ['m5_def(['instr_str']m5_NUM_INSTRS,
                                           m4_dquote(m4_str['']m4_substr(['                                        '], m4_len(m4_quote(m4_str)))))'])'])
  // Assemble an instruction.
  // m5_asm(FOO, ...) defines m4_inst# as m4_asm_FOO(...), counts instructions in m5_NUM_INSTRS ,and outputs a comment.
  def(NUM_INSTRS, 0)
  def(asm, ['m5_def(['instr']m5_NUM_INSTRS, m4_asm_$1(m4_shift($@)))['/']['/ Inst #']m5_NUM_INSTRS: $@m4_define(['m5_NUM_INSTRS'], m4_eval(m5_NUM_INSTRS + 1))'])

  //=========
// M4-generated code.
\m4
   m4_TLV_proc(riscv_gen, ['
      // The only output is for localparams, so
      // squash all output if no localparams.
      m4_out(m4_ifelse(m5_use_localparams, 0, [''], m4_dquote(m4_riscv_gen_guts())))
   '])
\m5
   fn(riscv_gen_guts, [
      // (Output is squashed by caller unless m5_use_localparams.)
      // v---------------------
      // Instruction characterization

      // M4 ugliness for instruction characterization.

      // For each opcode[6:2]
      // (User ISA Manual 2.2, Table 19.1)
      // Associate opcode[6:2] ([1:0] are 2'b11) with mnemonic and instruction type.
      // Instruction type is not in the table, but there seems to be a single instruction type for each of these,
      // so that is mapped here as well.
      // ~op5(bits, type, mnemonic)   // (produces localparam output for \SV_plus context)
      ~(['\SV_plus']m5_nl)
      ~op5(00000, I, LOAD)
      ~op5(00001, I, LOAD_FP)
      ~op5(00010, _, CUSTOM_0)
      ~op5(00011, _, MISC_MEM)
      ~op5(00100, I, OP_IMM)
      ~op5(00101, U, AUIPC)
      ~op5(00110, I, OP_IMM_32)
      ~op5(00111, _, 48B1)
      ~op5(01000, S, STORE)
      ~op5(01001, S, STORE_FP)
      ~op5(01010, _, CUSTOM_1)
      ~op5(01011, R, AMO)  // (R-type, but rs2 = const for some, based on funct7 which doesn't exist for I-type?? R-type w/ ignored R2?)
      ~op5(01100, R, OP)
      ~op5(01101, U, LUI)
      ~op5(01110, R, OP_32)
      ~op5(01111, _, 64B)
      ~op5(10000, R4, MADD)
      ~op5(10001, R4, MSUB)
      ~op5(10010, R4, NMSUB)
      ~op5(10011, R4, NMADD)
      ~op5(10100, R, OP_FP)  // (R-type, but rs2 = const for some, based on funct7 which doesn't exist for I-type?? R-type w/ ignored R2?)
      ~op5(10101, _, RESERVED_1)
      ~op5(10110, _, CUSTOM_2_RV128)
      ~op5(10111, _, 48B2)
      ~op5(11000, B, BRANCH)
      ~op5(11001, I, JALR)
      ~op5(11010, _, RESERVED_2)
      ~op5(11011, J, JAL)
      ~op5(11100, I, SYSTEM)
      ~op5(11101, _, RESERVED_3)
      ~op5(11110, _, CUSTOM_3_RV128)
      ~op5(11111, _, 80B)

      // For each instruction type, a mask, where each bit indicates whether
      // the op5 corresponding to the bit position is of the given type.
      // m5_instr calls produce localparam definitions for \SV_plus context)
      ~nl(\SV_plus)
      ~(m5_instr_types_sv(m5_instr_types_args))

      ~nl(['// Instruction characterization.'])
      ~nl(['// (User ISA Manual 2.2, Table 19.2)'])
      ~nl(\SV_plus)
      //nstr(type,  // (this is simply verified vs. op5)
      //     |  bit-width,
      //     |  |   extension, 
      //     |  |   |  opcode[6:2],  // (aka op5)
      //     |  |   |  |      func3,   // (if applicable)
      //     |  |   |  |      |    mnemonic
      ~instr(U, 32, I, 01101,      LUI)
      ~instr(U, 32, I, 00101,      AUIPC)
      ~instr(J, 32, I, 11011,      JAL)
      ~instr(I, 32, I, 11001, 000, JALR)
      ~instr(B, 32, I, 11000, 000, BEQ)
      ~instr(B, 32, I, 11000, 001, BNE)
      ~instr(B, 32, I, 11000, 100, BLT)
      ~instr(B, 32, I, 11000, 101, BGE)
      ~instr(B, 32, I, 11000, 110, BLTU)
      ~instr(B, 32, I, 11000, 111, BGEU)
      ~instr(I, 32, I, 00000, 000, LB)
      ~instr(I, 32, I, 00000, 001, LH)
      ~instr(I, 32, I, 00000, 010, LW)
      ~instr(I, 32, I, 00000, 100, LBU)
      ~instr(I, 32, I, 00000, 101, LHU)
      ~instr(S, 32, I, 01000, 000, SB)
      ~instr(S, 32, I, 01000, 001, SH)
      ~instr(S, 32, I, 01000, 010, SW)
      ~instr(I, 32, I, 00100, 000, ADDI)
      ~instr(I, 32, I, 00100, 010, SLTI)
      ~instr(I, 32, I, 00100, 011, SLTIU)
      ~instr(I, 32, I, 00100, 100, XORI)
      ~instr(I, 32, I, 00100, 110, ORI)
      ~instr(I, 32, I, 00100, 111, ANDI)
      ~instr(If, 32, I, 00100, 001, 000000, SLLI)
      ~instr(If, 32, I, 00100, 101, 000000, SRLI)
      ~instr(If, 32, I, 00100, 101, 010000, SRAI)
      ~instr(R, 32, I, 01100, 000, 0000000, ADD)
      ~instr(R, 32, I, 01100, 000, 0100000, SUB)
      ~instr(R, 32, I, 01100, 001, 0000000, SLL)
      ~instr(R, 32, I, 01100, 010, 0000000, SLT)
      ~instr(R, 32, I, 01100, 011, 0000000, SLTU)
      ~instr(R, 32, I, 01100, 100, 0000000, XOR)
      ~instr(R, 32, I, 01100, 101, 0000000, SRL)
      ~instr(R, 32, I, 01100, 101, 0100000, SRA)
      ~instr(R, 32, I, 01100, 110, 0000000, OR)
      ~instr(R, 32, I, 01100, 111, 0000000, AND)
      //~instr(_, 32, I, 00011, 000, FENCE)
      //~instr(_, 32, I, 00011, 001, FENCE_I)
      //~instr(_, 32, I, 11100, 000, ECALL_EBREAK)  // Two instructions distinguished by an immediate bit, treated as a single instruction.
      ~instr(I, 32, I, 11100, 001, CSRRW)
      ~instr(I, 32, I, 11100, 010, CSRRS)
      ~instr(I, 32, I, 11100, 011, CSRRC)
      ~instr(I, 32, I, 11100, 101, CSRRWI)
      ~instr(I, 32, I, 11100, 110, CSRRSI)
      ~instr(I, 32, I, 11100, 111, CSRRCI)
      ~instr(I, 64, I, 00000, 110, LWU)
      ~instr(I, 64, I, 00000, 011, LD)
      ~instr(S, 64, I, 01000, 011, SD)
      ~instr(If, 64, I, 00100, 001, 000000, SLLI)
      ~instr(If, 64, I, 00100, 101, 000000, SRLI)
      ~instr(If, 64, I, 00100, 101, 010000, SRAI)
      ~instr(I, 64, I, 00110, 000, ADDIW)
      ~instr(If, 64, I, 00110, 001, 000000, SLLIW)
      ~instr(If, 64, I, 00110, 101, 000000, SRLIW)
      ~instr(If, 64, I, 00110, 101, 010000, SRAIW)
      ~instr(R, 64, I, 01110, 000, 0000000, ADDW)
      ~instr(R, 64, I, 01110, 000, 0100000, SUBW)
      ~instr(R, 64, I, 01110, 001, 0000000, SLLW)
      ~instr(R, 64, I, 01110, 101, 0000000, SRLW)
      ~instr(R, 64, I, 01110, 101, 0100000, SRAW)
      ~instr(R, 32, M, 01100, 000, 0000001, MUL)
      ~instr(R, 32, M, 01100, 001, 0000001, MULH)
      ~instr(R, 32, M, 01100, 010, 0000001, MULHSU)
      ~instr(R, 32, M, 01100, 011, 0000001, MULHU)
      ~instr(R, 32, M, 01100, 100, 0000001, DIV)
      ~instr(R, 32, M, 01100, 101, 0000001, DIVU)
      ~instr(R, 32, M, 01100, 110, 0000001, REM)
      ~instr(R, 32, M, 01100, 111, 0000001, REMU)
      ~instr(R, 64, M, 01110, 000, 0000001, MULW)
      ~instr(R, 64, M, 01110, 100, 0000001, DIVW)
      ~instr(R, 64, M, 01110, 101, 0000001, DIVUW)
      ~instr(R, 64, M, 01110, 110, 0000001, REMW)
      ~instr(R, 64, M, 01110, 111, 0000001, REMUW)
      ~instr(I, 32, F, 00001, 010, FLW)
      ~instr(S, 32, F, 01001, 010, FSW)
      ~instr(R4, 32, F, 10000, rm, 00, FMADDS)
      ~instr(R4, 32, F, 10001, rm, 00, FMSUBS)
      ~instr(R4, 32, F, 10010, rm, 00, FNMSUBS)
      ~instr(R4, 32, F, 10011, rm, 00, FNMADDS)
      ~instr(R, 32, F, 10100, rm, 0000000, FADD_S)
      ~instr(R, 32, F, 10100, rm, 0000100, FSUB_S)
      ~instr(R, 32, F, 10100, rm, 0001000, FMULS)
      ~instr(R, 32, F, 10100, rm, 0001100, FDIVS)
      ~instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRTS)
      ~instr(R, 32, F, 10100, 000, 0010000, FSGNJS)
      ~instr(R, 32, F, 10100, 001, 0010000, FSGNJNS)
      ~instr(R, 32, F, 10100, 010, 0010000, FSGNJXS)
      ~instr(R, 32, F, 10100, 000, 0010100, FMINS)
      ~instr(R, 32, F, 10100, 001, 0010100, FMAXS)
      ~instr(R2, 32, F, 10100, rm, 1100000, 00000, FCVTWS)
      ~instr(R2, 32, F, 10100, rm, 1100000, 00001, FCVTWUS)
      ~instr(R2, 32, F, 10100, 000, 1110000, 00000, FMVXW)
      ~instr(R, 32, F, 10100, 010, 1010000, FEQS)
      ~instr(R, 32, F, 10100, 001, 1010000, FLTS)
      ~instr(R, 32, F, 10100, 000, 1010000, FLES)
      ~instr(R2, 32, F, 10100, 001, 1110000, 00000, FCLASSS)
      ~instr(R2, 32, F, 10100, rm, 1101000, 00000, FCVTSW)
      ~instr(R2, 32, F, 10100, rm, 1101000, 00001, FCVTSWU)
      ~instr(R2, 32, F, 10100, 000, 1111000, 00000, FMVWX)
      ~instr(R2, 64, F, 10100, rm, 1100000, 00010, FCVTLS)
      ~instr(R2, 64, F, 10100, rm, 1100000, 00011, FCVTLUS)
      ~instr(R2, 64, F, 10100, rm, 1101000, 00010, FCVTSL)
      ~instr(R2, 64, F, 10100, rm, 1101000, 00011, FCVTSLU)
      ~instr(I, 32, D, 00001, 011, FLD)
      ~instr(S, 32, D, 01001, 011, FSD)
      ~instr(R4, 32, D, 10000, rm, 01, FMADDD)
      ~instr(R4, 32, D, 10001, rm, 01, FMSUBD)
      ~instr(R4, 32, D, 10010, rm, 01, FNMSUBD)
      ~instr(R4, 32, D, 10011, rm, 01, FNMADDD)
      ~instr(R, 32, D, 10100, rm, 0000001, FADD_D)
      ~instr(R, 32, D, 10100, rm, 0000101, FSUB_D)
      ~instr(R, 32, D, 10100, rm, 0001001, FMUL_D)
      ~instr(R, 32, D, 10100, rm, 0001101, FDIVD)
      ~instr(R2, 32, D, 10100, rm, 0101101, 00000, FSQRTD)
      ~instr(R, 32, D, 10100, 000, 0010001, FSGNJD)
      ~instr(R, 32, D, 10100, 001, 0010001, FSGNJND)
      ~instr(R, 32, D, 10100, 010, 0010001, FSGNJXD)
      ~instr(R, 32, D, 10100, 000, 0010101, FMIND)
      ~instr(R, 32, D, 10100, 001, 0010101, FMAXD)
      ~instr(R2, 32, D, 10100, rm, 0100000, 00001, FCVTSD)
      ~instr(R2, 32, D, 10100, rm, 0100001, 00000, FCVTDS)
      ~instr(R, 32, D, 10100, 010, 1010001, FEQD)
      ~instr(R, 32, D, 10100, 001, 1010001, FLTD)
      ~instr(R, 32, D, 10100, 000, 1010001, FLED)
      ~instr(R2, 32, D, 10100, 001, 1110001, 00000, FCLASSD)
      ~instr(R2, 32, D, 10100, rm, 1110001, 00000, FCVTWD)
      ~instr(R2, 32, D, 10100, rm, 1100001, 00001, FCVTWUD)
      ~instr(R2, 32, D, 10100, rm, 1101001, 00000, FCVTDW)
      ~instr(R2, 32, D, 10100, rm, 1101001, 00001, FCVTDWU)
      ~instr(R2, 64, D, 10100, rm, 1100001, 00010, FCVTLD)
      ~instr(R2, 64, D, 10100, rm, 1100001, 00011, FCVTLUD)
      ~instr(R2, 64, D, 10100, 000, 1110001, 00000, FMVXD)
      ~instr(R2, 64, D, 10100, rm, 1101001, 00010, FCVTDL)
      ~instr(R2, 64, D, 10100, rm, 1101001, 00011, FCVTDLU)
      ~instr(R2, 64, D, 10100, 000, 1111001, 00000, FMVDX)
      ~instr(I, 32, Q, 00001, 100, FLQ)
      ~instr(S, 32, Q, 01001, 100, FSQ)
      ~instr(R4, 32, Q, 10000, rm, 11, FMADDQ)
      ~instr(R4, 32, Q, 10001, rm, 11, FMSUBQ)
      ~instr(R4, 32, Q, 10010, rm, 11, FNMSUBQ)
      ~instr(R4, 32, Q, 10011, rm, 11, FNMADDQ)
      ~instr(R, 32, Q, 10100, rm, 0000011, FADD_Q)
      ~instr(R, 32, Q, 10100, rm, 0000111, FSUB_Q)
      ~instr(R, 32, Q, 10100, rm, 0001011, FMUL_Q)
      ~instr(R, 32, Q, 10100, rm, 0001111, FDIVQ)
      ~instr(R2, 32, Q, 10100, rm, 0101111, 00000, FSQRT_Q)
      ~instr(R, 32, Q, 10100, 000, 0010011, FSGNJ_Q)
      ~instr(R, 32, Q, 10100, 001, 0010011, FSGNJN_Q)
      ~instr(R, 32, Q, 10100, 010, 0010011, FSGNJX_Q)
      ~instr(R, 32, Q, 10100, 000, 0010111, FMIN_Q)
      ~instr(R, 32, Q, 10100, 001, 0010111, FMAX_Q)
      ~instr(R2, 32, Q, 10100, rm, 0100000, 00011, FCVT_S_Q)
      ~instr(R2, 32, Q, 10100, rm, 0100011, 00000, FCVT_Q_S)
      ~instr(R2, 32, Q, 10100, rm, 0100001, 00011, FCVT_D_Q)
      ~instr(R2, 32, Q, 10100, rm, 0100011, 00001, FCVT_Q_D)
      ~instr(R, 32, Q, 10100, 010, 1010011, FEQ_Q)
      ~instr(R, 32, Q, 10100, 001, 1010011, FLT_Q)
      ~instr(R, 32, Q, 10100, 000, 1010011, FLE_Q)
      ~instr(R2, 32, Q, 10100, 001, 1110011, 00000, FCLASS_Q)
      ~instr(R2, 32, Q, 10100, rm, 1110011, 00000, FCVT_W_Q)
      ~instr(R2, 32, Q, 10100, rm, 1100011, 00001, FCVT_WU_Q)
      ~instr(R2, 32, Q, 10100, rm, 1101011, 00000, FCVT_Q_W)
      ~instr(R2, 32, Q, 10100, rm, 1101011, 00001, FCVT_Q_WU)
      ~instr(R2, 64, Q, 10100, rm, 1100011, 00010, FCVT_L_Q)
      ~instr(R2, 64, Q, 10100, rm, 1100011, 00011, FCVT_L_UQ)
      ~instr(R2, 64, Q, 10100, rm, 1101011, 00010, FCVT_Q_L)
      ~instr(R2, 64, Q, 10100, rm, 1101011, 00011, FCVT_Q_LU)
      ~instr(R2, 32, A, 01011, 010, 00010, 00000, LR_W)
      ~instr(R, 32, A, 01011, 010, 00011, SC_W)
      ~instr(R, 32, A, 01011, 010, 00001, AMOSWAP_W)
      ~instr(R, 32, A, 01011, 010, 00000, AMOADD_W)
      ~instr(R, 32, A, 01011, 010, 00100, AMOXOR_W)
      ~instr(R, 32, A, 01011, 010, 01100, AMOAND_W)
      ~instr(R, 32, A, 01011, 010, 01000, AMOOR_W)
      ~instr(R, 32, A, 01011, 010, 10000, AMOMIN_W)
      ~instr(R, 32, A, 01011, 010, 10100, AMOMAX_W)
      ~instr(R, 32, A, 01011, 010, 11000, AMOMIN_UW)
      ~instr(R, 32, A, 01011, 010, 11100, AMOMAX_UW)
      ~instr(R2, 64, A, 01011, 011, 00010, 00000, LR_D)
      ~instr(R, 64, A, 01011, 011, 00011, SC_D)
      ~instr(R, 64, A, 01011, 011, 00001, AMOSWAP_D)
      ~instr(R, 64, A, 01011, 011, 00000, AMOADD_D)
      ~instr(R, 64, A, 01011, 011, 00100, AMOXOR_D)
      ~instr(R, 64, A, 01011, 011, 01100, AMOAND_D)
      ~instr(R, 64, A, 01011, 011, 01000, AMOOR_D)
      ~instr(R, 64, A, 01011, 011, 10000, AMOMIN_D)
      ~instr(R, 64, A, 01011, 011, 10100, AMOMAX_D)
      ~instr(R, 64, A, 01011, 011, 11000, AMOMIN_UD)
      ~instr(R, 64, A, 01011, 011, 11100, AMOMAX_UD)
      ~instr(R, 32, B, 01100, 111, 0100000, ANDN)
      ~instr(R, 32, B, 01100, 110, 0100000, ORN)
      ~instr(R, 32, B, 01100, 100, 0100000, XNOR)
      ~instr(R, 32, B, 01100, 001, 0010000, SLO)
      ~instr(R, 32, B, 01100, 101, 0010000, SRO)
      ~instr(R, 32, B, 01100, 001, 0110000, ROL)
      ~instr(R, 32, B, 01100, 101, 0110000, ROR)
      ~instr(R, 32, B, 01100, 001, 0100100, SBCLR)
      ~instr(R, 32, B, 01100, 001, 0010100, SBSET)
      ~instr(R, 32, B, 01100, 001, 0110100, SBINV)
      ~instr(R, 32, B, 01100, 101, 0100100, SBEXT)
      ~instr(R, 32, B, 01100, 101, 0010100, GORC)
      ~instr(R, 32, B, 01100, 101, 0110100, GREV)
      ~instr(If, 32, B, 00100, 001, 00100, SLOI)
      ~instr(If, 32, B, 00100, 101, 00100, SROI)
      ~instr(If, 32, B, 00100, 101, 01100, RORI)
      ~instr(If, 32, B, 00100, 001, 01001, SBCLRI)
      ~instr(If, 32, B, 00100, 001, 00101, SBSETI)
      ~instr(If, 32, B, 00100, 001, 01101, SBINVI)
      ~instr(If, 32, B, 00100, 101, 01001, SBEXTI)
      ~instr(If, 32, B, 00100, 101, 00101, GORCI)
      ~instr(If, 32, B, 00100, 101, 01101, GREVI)
      //~instr(R4, 32, B, 01100, 001, 11, CMIX)  // Currently we are excluing ternary BMI's for optimization, although its only a draft
      //~instr(R4, 32, B, 01100, 101, 11, CMOV)
      //~instr(R4, 32, B, 01100, 001, 10, FSL)
      //~instr(R4, 32, B, 01100, 101, 10, FSR)
      //~instr(R4, 32, B, 00100, 101, 10, FSRI)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00000, CLZ) // single operand instruction are currently using R2-type rather than I-type encoding
      ~instr(R2, 32, B, 01100, 001, 0110000, 00001, CTZ)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00010, PCNT)
      //~instr(R2, 64, B, 01100, 001, 0110000, 00011, BMATFLIP)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00100, SEXT_B)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00101, SEXT_H)
      ~instr(R2, 32, B, 01100, 001, 0110000, 10000, CRC32_B)
      ~instr(R2, 32, B, 01100, 001, 0110000, 10001, CRC32_H)
      ~instr(R2, 32, B, 01100, 001, 0110000, 10010, CRC32_W)
      //~instr(R2, 64, B, 01100, 001, 0110000, 10011, CRC32_D)
      ~instr(R2, 32, B, 01100, 001, 0110000, 11000, CRC32C_B)
      ~instr(R2, 32, B, 01100, 001, 0110000, 11001, CRC32C_H)
      ~instr(R2, 32, B, 01100, 001, 0110000, 11010, CRC32C_W)
      //~instr(R2, 64, B, 01100, 001, 0110000, 11011, CRC32C_D)
      ~instr(R, 32, B, 01100, 001, 0000101, CLMUL)
      ~instr(R, 32, B, 01100, 010, 0000101, CLMULR)
      ~instr(R, 32, B, 01100, 011, 0000101, CLMULH)
      ~instr(R, 32, B, 01100, 100, 0000101, MIN)
      ~instr(R, 32, B, 01100, 101, 0000101, MAX)
      ~instr(R, 32, B, 01100, 110, 0000101, MINU)
      ~instr(R, 32, B, 01100, 111, 0000101, MAXU)
      ~instr(R, 32, B, 01100, 001, 0000100, SHFL)
      ~instr(R, 32, B, 01100, 101, 0000100, UNSHFL)
      ~instr(R, 32, B, 01100, 110, 0100100, BDEP)
      ~instr(R, 32, B, 01100, 110, 0000100, BEXT)
      ~instr(R, 32, B, 01100, 100, 0000100, PACK)
      ~instr(R, 32, B, 01100, 100, 0100100, PACKU)
      //~instr(R, 64, B, 01100, 011, 0000100, BMATOR)
      //~instr(R, 64, B, 01100, 011, 0100100, BMATXOR)
      ~instr(R, 32, B, 01100, 111, 0000100, PACKH)
      ~instr(R, 32, B, 01100, 111, 0100100, BFP)
      ~instr(If, 32, B, 00100, 001, 000010, SHFLI)
      ~instr(If, 32, B, 00100, 101, 000010, UNSHFLI)
      //~instr(I, 64, B, 01100, 100, ADDIWU)
      //~instr(If, 64, B, 01100, 001, 000010, SLLI_UW)
      //~instr(R, 64, B, 01110, 000, 0000101, ADDWU)
      //~instr(R, 64, B, 01110, 000, 0100101, SUBWU)
      //~instr(R, 64, B, 01110, 000, 0000100, ADD_UW)
      //~instr(R, 64, B, 01110, 000, 0100100, SUBU_W)
      //~instr(R, 64, B, 01110, 001, 0010000, SLOW)
      //~instr(R, 64, B, 01110, 101, 0010000, SROW)
      //~instr(R, 64, B, 01110, 001, 0110000, ROLW)
      //~instr(R, 64, B, 01110, 101, 0110000, RORW)
      //~instr(R, 64, B, 01110, 001, 0100100, SBCLRW)
      //~instr(R, 64, B, 01110, 001, 0010100, SBSETW)
      //~instr(R, 64, B, 01110, 001, 0110100, SBINVW)
      //~instr(R, 64, B, 01110, 101, 0100100, SBEXTW)
      //~instr(R, 64, B, 01110, 101, 0010100, GROCW)
      //~instr(R, 64, B, 01110, 101, 0110100, GREVW)
      //~instr(If, 64, B, 00110, 001, 001000, SLOIW)
      //~instr(If, 64, B, 00110, 101, 001000, SROIW)
      //~instr(If, 64, B, 00110, 101, 011000, RORIW)
      //~instr(If, 64, B, 00110, 001, 010010, SBCLRIW)
      //~instr(If, 64, B, 00110, 001, 001010, SBSETIW)
      //~instr(If, 64, B, 00110, 001, 011010, SBINVIW)
      //~instr(If, 64, B, 00110, 101, 001010, GORCIW)
      //~instr(If, 64, B, 00110, 101, 011010, GREVIW)
      //~instr(R4, 64, B, 01110, 001, 10, FSLW)
      //~instr(R4, 64, B, 01110, 101, 10, FSRW)
      //~instr(R4, 64, B, 00110, 101, 10, FSRIW) //rs2 is "imm"
      //~instr(R2, 64, B, 00110, 001, 0110000, 00000, CLZW)
      //~instr(R2, 64, B, 00110, 001, 0110000, 00001, CTZW)
      //~instr(R2, 64, B, 00110, 001, 0110000, 00010, PCNTW)
      //~instr(R, 64, B, 01110, 001, 0000101, CLMULW)
      //~instr(R, 64, B, 01110, 010, 0000101, CLMULRW)
      //~instr(R, 64, B, 01110, 011, 0000101, CLMULHW)
      //~instr(R, 64, B, 01110, 001, 0000100, SHFLW)
      //~instr(R, 64, B, 01110, 101, 0000100, UNSHFLW)
      //~instr(R, 64, B, 01110, 110, 0100100, BDEPW)
      //~instr(R, 64, B, 01110, 110, 0000100, BEXTW)
      //~instr(R, 64, B, 01110, 100, 0000100, PACKW)
      //~instr(R, 64, B, 01110, 100, 0100100, PACKUW)
      //~instr(R, 64, B, 01110, 111, 0100100, BFPW)
   ])



   // =========
   // Assembler
   // =========
   
   // These functions go from proper RISC-V assembly code to the pseudo-assembly defined by m5_asm functions.
   
   fn(abi_to_reg: ['Map an ABI register name. ABI name -> register name. If field_type is given,
                    the type is checked and only the index is returned; o.w. the type is returned
                    as the first character, e.g. "x3". For unknown registers, type is "-" and
                    a register index is returned that is in range, though meaningless.'],
      abi: ['ABI or register name'],
      ?field_type: ['[x|f] register type'],
   {
      // Helpers
      macro(unknown,
         ['m5_error(['Unrecognized ABI register ']m5_abi)'])
      // Check that m5_num was found in ABI and $1 <= m5_num <= $2.
      macro(num_range,
         ['m5_if(m5_eq(m5_num, ['']) || (m5_num < ['<$1,2>']) || (m5_num > ['<$2,2>']), ['m5_unknown()'])'])
      // Set and check m5_reg_type.
      macro(type,
         ['m5_set(reg_type, ['<$1,2>'])m5_if(m5_neq(m5_field_type, ['']) && m5_neq(m5_reg_type, m5_field_type), ['m5_error(['ABI register "']m5_abi['" of type ']m5_reg_type[' used where type ']m5_field_type[' is expected.'])'])'])
      
      var(reg_type, -)  // The type [x|f] corresponding to m5_abi, set by m5_type.
      var(reg_index, 0)  // Register index, defaulted to 0.
      // Parse ABI reg name.
      var_regex(m5_abi, ['\([a-z]+\)\([0-9]*\)'], name, num)
      else({
         unknown()
         // Assume zero
         set(name, zero)
      })
      set(reg_index, m5_case(m5_name,
         x, [
            type(x)
            num_range(0, 31)
            ~num
         ],
         f, [
            type(f)
            num_range(0, 31)
            ~num
         ],
         a, [
            type(x)
            num_range(0, 7)
            ~calc(m5_num + 10)
         ],
         s, [
            type(x)
            num_range(0, 11)
            ~calc(m5_num + m5_if(m5_num < 2, 8, 16))
         ],
         t, [
            type(x)
            num_range(0, 6)
            ~calc(m5_num + m5_if(m5_num < 3, 5, 25))
         ],
         ft, [
            type(f)
            num_range(0, 7)
            ~calc(m5_num + m5_if(m5_num < 8, 0, 20))
         ],
         fs, [
            type(f)
            num_range(0, 11)
            ~calc(m5_num + m5_if(m5_num < 2, 8, 16))
         ],
         fa, [
            type(f)
            num_range(0, 7)
            ~calc(m5_num + 10)
         ], [
            // Not a ranged type.
            // Shouldn't have a range.
            ~ifeq(m5_num, [''], [''], ['m5_unknown()['0']'])
            // Handle x types.
            ~case(m5_name,
               zero, 0,
               ra, 1,
               sp, 2,
               gp, 3,
               tp, 4)
            if_so([
               type(x)
            ])
            // Handle f types (and unrecognized).
            ~else([
               ~case(m5_name,
                  fp, 8)
               if_so([
                  type(f)
               ])
               ~else([
                  unknown()
                  ~(0)
               ])
            ])
         ]))
      // Return type.
      ~if_null(field_type, m5_reg_type)
      // Return index in the range 0..32 even if unknown.
      ~if_null(reg_index, 0, [
         ~if(m5_reg_index >= 32, 31, m5_reg_index)
      ])
   })
   
   
   fn(assemble_line, str, {
      
      /// Strip comment and trailing whitespace from m5_str.
      var(pos, m5_index(m5_str, ['#']))
      if(m5_pos >= 0, [
         set(str, m5_substr(m5_str, 0, m5_pos))
      ])
      strip_trailing_whitespace_from(str)
      
      /// Parse (uncommented) line.
      if_regex(m5_str, ['['^\(\s*\)$'], dummy'], {
         /// Empty line
         DEBUG(['Found empty line: '])
      }, ['['^\s+\(\w+\)\s+\(.*\)'], instr, args'], {
         /// Instruction
         DEBUG(['Found instruction: ']m5_instr m5_args)
         var(op5, ...)
         if(m5_eq(m5_op5_named_LOAD_FP) || m5_eq(m5_op5_named_LOAD_FP), [
            
         ])
      }, ['['^\.\(\w+\)\(\)?'], directive, args'], {
         /// Directive
         DEBUG(['Found directive: ']m5_directive m5_args)
      }, ['['^\(\w+\):\s*$'], label'], {
         /// Label
         DEBUG(['Found label: ']m5_label)
      }, {
         error(['Could not parse assembly code line: "']m5_str['"'])
      })
   })
