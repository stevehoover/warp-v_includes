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
     ~(['   ']m5_define_localparam(['OP5_$3'], ['[4:0]'], ['5'b$1'])m5_nl)
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
     ifne(m5_OP5_$4_TYPE, m5_ifdef(['m5_instr_type_of_$1'], m5_instr_type_of_$1, ['$1']),
        ['m5_errprint(['Instruction ']m5_mnemonic[''s type (']m5_type[') is inconsistant with its op5 code (']m5_op5[') of type ']m5_eval(['m5_OP5_']m5_op5['_TYPE'])[' on line ']m4___line__[' of file ']m4_FILE.m5_nl)'])
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
  macro(asm_funct3, ['['m4_ifelse($2, ['rm'], ['3'b']m4_rm, m5_localparam_value(['$1_INSTR_FUNCT3']))']'])
  
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
     =m5_instr_decode_expr(m5_mnemonic, ['$raw_op5 == 5'b']m5_op5)
     =m4_null()
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
  //   m5_instr(U, 32, I, 01101,      LUI)
  //   m5_instr(J, 32, I, 11011,      JAL)
  //   m5_instr(B, 32, I, 11000, 000, BEQ)
  //   m5_instr(S, 32, I, 01000, 000, SB)
  //   m5_instr(I, 32, I, 00100, 000, ADDI)
  //   m5_instr(If, 64, I, 00100, 101, 000000, SRLI)  // (imm[11:6] are used like funct7[6:1] and must be 000000)
  //   m5_instr(R, 32, I, 01100, 000, 0000000, ADD)
  //   m5_instr(R4, 32, F, 10000, rm, 10, FMADD.D)
  //   m5_instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRT.S)
  //   m5_instr(R2, 32, A, 01011, 010, 00010, 00000, LR.W)  // (5 bits for funct7 for all "A"-ext instrs)
  //   m5_instr(R, 32, A, 01011, 010, 00011, SC.W)          //   "
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
        asm_instr_str(I, m5_mnemonic, m4_func_args)
        ~(['{12'b']m5_imm[', ']m4_asm_reg(m5_src1)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT3'])[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['})
     })
  })
  fn(instrIf, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct7($@, m5_mnemonic, m4_len(m5_imm_funct))
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^imm_funct, ^mnemonic, {
        asm_instr_str(I, m5_mnemonic, m4_func_args)
        ~(['{']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT']m4_len(m5_imm_funct))[', ']m4_eval(12-m4_len(m5_imm_funct))[''b']m5_imm[', ']m4_asm_reg(m5_src1)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT3'])[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['})
     })
  })
  fn(instrR, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct7($@, m5_mnemonic, m4_ifelse(m5_ext, ['A'], 5, 7))
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]src2, ?[4]rm, ^ext, ^funct3, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m4_func_args)
        ~(['{']m4_ifelse(m5_ext, ['A'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT5'])[', ']m5_src1'], m5_localparam_value(m5_mnemonic['_INSTR_FUNCT7']))[', ']m4_asm_reg(m5_src2)[', ']m4_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrR2, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, [6]fixed_src2, ..., {
     ~instr_funct7($@, 7, m5_fixed_src2)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, ?[3]rm, ^ext, ^funct3, ^fixed_src2, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m4_func_args)
        ~(['{']m4_ifelse(m5_ext, ['A'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT5'])[', ']m5_src1'], m5_localparam_value(m5_mnemonic['_INSTR_FUNCT7']))[', 5'b']m5_fixed_src2[', ']m4_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrR4, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct2($@)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]src2, [4]src3, ?[5]rm, ^funct3, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m4_func_args)
        ~(['{']m4_asm_reg(m5_src3)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT2'])[', ']m4_asm_reg(m5_src2)[', ']m4_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrS, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@, ['no_dest'])
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^mnemonic, {
        asm_instr_str(S, m5_mnemonic, m4_func_args)
        ~(['{']m4_asm_imm_field(m5_imm, 12, 11, 5)[', ']m4_asm_reg(m5_src1)[', ']m4_asm_reg(m5_dest)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m4_asm_imm_field(m5_imm, 12, 4, 0)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrB, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@, no_dest)
     fn(['asm_']m5_mnemonic, [1]src1, [2]src2, [3]target, ^funct3, ^mnemonic, {
        def(imm, m4_asm_target(13))
        asm_instr_str(B, m5_mnemonic, m4_func_args)
        ~({m4_asm_imm_field(m5_imm, 13, 12, 12)[', ']m4_asm_imm_field(m5_imm, 13, 10, 5)[', ']m4_asm_reg(m5_src2)[', ']m4_asm_reg(m5_src1)[', ']']m5_asm_funct3(m5_mnemonic, m5_funct3)['[', ']m4_asm_imm_field(m5_imm, 13, 4, 1)[', ']m4_asm_imm_field(m5_imm, 13, 11, 11)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['})
     })
  })
  fn(instrU, mnemonic, [1]width, [2]ext, [3]op5, ..., {
     ~instr_no_func(m5_mnemonic, m5_op5)
     fn(['asm_']m5_mnemonic, [1]dest, [2]imm, ^mnemonic, {
        asm_instr_str(U, m5_mnemonic, m4_func_args)
        ~(['{']m4_asm_imm_field(m5_imm, 20, 19, 0)[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instrJ, mnemonic, [1]width, [2]ext, [3]op5, ..., {
     ~instr_no_func(m5_mnemonic, m5_op5)
     fn(['asm_']m5_mnemonic, [1]dest, [2]target, ^mnemonic, {
        def(imm, m4_asm_target(21))
        asm_instr_str(J, m5_mnemonic, m4_func_args)
        ~(['{']m4_asm_imm_field(m5_imm, 21, 20, 20)[', ']m4_asm_imm_field(m5_imm, 21, 10, 1)[', ']m4_asm_imm_field(m5_imm, 21, 11, 11)[', ']m4_asm_imm_field(m5_imm, 21, 19, 12)[', ']m4_asm_reg(m5_dest)[', ']']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['['}'])
     })
  })
  fn(instr_, mnemonic, [1]width, [2]ext, [3]op5, {
     ~instr_no_func(m5_mnemonic, m5_op5)
  })
\m4

  // For each instruction type.
  // Declare localparam[31:0] INSTR_TYPE_X_MASK, initialized to 0 that will be given a 1 bit for each op5 value of its type.
  m5_def(instr_types_args, ['I, R, R2, R4, S, B, J, U, _'])
  m5_instr_types(m5_instr_types_args)


  // Instruction fields (User ISA Manual 2.2, Fig. 2.2)
  // TODO: m5_define_fields doesn't exist, but this didn't break WARP-V?
  m5_define_fields(INSTR, 32, FUNCT7, 25, RS2, 20, RS1, 15, FUNCT3, 12, RD, 7, OP5, 2, OP2, 0)

  //=========
  // Specifically for assembler.

  // For labels:
  // m4_[un]signed_int_to_fixed_binary(digits, value)
  // Returns a string of 0s and 1s, representing the zero-padded binary value.
  // Args:
  //   digits: number of binary digits
  //   value: value (for signed, must be in the range -2**digits .. 2**digits-1,
  //                 for unsigned, must be in the range 0 .. 2**digits)
  m4_def(unsigned_int_to_fixed_binary, ['m4_ifelse(m4_eval($1 > 1), 1, ['m4_unsigned_int_to_fixed_binary(m4_eval($1-1), m4_eval($2 >> 1))'])['']m4_eval($2 % 2)'])
  m4_def(signed_int_to_fixed_binary, ['m4_unsigned_int_to_fixed_binary(['$1'], m4_ifelse(m4_eval($2 >= 0), 1, ['$2'], ['m4_eval($2 + 2 ** $1)']))'])
  m4_def(label, ['m4_def(label_$1_addr, m5_NUM_INSTRS)'])
  // m4_label_to_imm(label, bit-width): Convert a label (excluding :) to an immediate for current m5_NUM_INSTRS.
  m4_def(label_to_imm, ['m4_signed_int_to_fixed_binary($2, m4_ifdef(['m4_label_$1_addr'], ['m4_eval((m4_label_$1_addr - m5_NUM_INSTRS) * 4)'], ['m4_errprint(['No assembler label "']$1['"']m4_nl)0000000000000']))'])
  
  // m4_asm_target(width): Output the offset for a given branch target arg in m4_target of the form :label or 1111111111000, with the given bit width.
  m4_def(asm_target, ['m4_ifelse(m5_extract_prefix_eval([':'], target), [':'], ['m4_label_to_imm(m5_target, $1)'], ['m5_target'])'])


  // An 20-bit immediate binary zero string.
  m4_def(asm_imm_zero, ['00000000000000000000'])
  // Zero-extend to n bits. E.g. m4_asm_zero_ext(1001, 7) => 0001001
  m4_def(asm_zero_ext, ['m4_substr(m4_asm_imm_zero, 0, m4_eval($2 - m4_len($1)))$1'])
  // Extract bits from a binary immediate value.
  // m4_asm_imm_field(binary-imm, imm-length, max-bit, min-bit)
  // E.g. m4_asm_imm_field(101011, 17, 7, 3) => 5'b00101
  m4_def(asm_imm_field, ['m4_eval($3 - $4 + 1)'b['']m4_substr(m4_asm_zero_ext($1, $2), m4_eval($2 - $3 - 1), m4_eval($3 - $4 + 1))'])
  // Register operand.
  m4_def(asm_reg, ['m4_ifelse(m4_substr(['$1'], 0, 1), ['r'], [''], ['m4_ifelse(m4_substr(['$1'], 0, 1), ['x'], [''], ['m4_errprint(['$1 passed to register field.'])'])'])5'd['']m4_substr(['$1'], 1)'])

  // For debug, a string for an asm instruction.
  m5_def(asm_instr_str, ['m4_with(str, ['['($1) $2 ']']m4_dquote(m4_shift(m4_shift($@))),
                                  ['m5_def(['instr_str']m5_NUM_INSTRS,
                                           m4_dquote(m4_str['']m4_substr(['                                        '], m4_len(m4_quote(m4_str)))))'])'])
  // Assemble an instruction.
  // m5_asm(FOO, ...) defines m4_inst# as m4_asm_FOO(...), counts instructions in m5_NUM_INSTRS ,and outputs a comment.
  m5_def(NUM_INSTRS, 0)
  m5_def(asm, ['m5_def(['instr']m5_NUM_INSTRS, m4_asm_$1(m4_shift($@)))['/']['/ Inst #']m5_NUM_INSTRS: $@m4_define(['m5_NUM_INSTRS'], m4_eval(m5_NUM_INSTRS + 1))'])

  //=========
// M4-generated code.
\m4
   m4_TLV_proc(riscv_gen, ['
      // The only output is for localparams, so
      // squash all output if no localparams.
      m4_out(m4_ifelse(m5_use_localparams, 0, [''], m4_dquote(m4_riscv_gen_guts())))
   '])
   m4_proc(riscv_gen_guts, ['
      // (Output is squashed by caller unless m5_use_localparams.)
      // v---------------------
      // Instruction characterization

      // M4 ugliness for instruction characterization.

      // For each opcode[6:2]
      // (User ISA Manual 2.2, Table 19.1)
      // Associate opcode[6:2] ([1:0] are 2'b11) with mnemonic and instruction type.
      // Instruction type is not in the table, but there seems to be a single instruction type for each of these,
      // so that is mapped here as well.
      // op5(bits, type, mnemonic)   // (produces localparam output for \SV_plus context)
      m4_out_nl(\SV_plus)
      m5_op5(00000, I, LOAD)
      m5_op5(00001, I, LOAD_FP)
      m5_op5(00010, _, CUSTOM_0)
      m5_op5(00011, _, MISC_MEM)
      m5_op5(00100, I, OP_IMM)
      m5_op5(00101, U, AUIPC)
      m5_op5(00110, I, OP_IMM_32)
      m5_op5(00111, _, 48B1)
      m5_op5(01000, S, STORE)
      m5_op5(01001, S, STORE_FP)
      m5_op5(01010, _, CUSTOM_1)
      m5_op5(01011, R, AMO)  // (R-type, but rs2 = const for some, based on funct7 which doesn't exist for I-type?? R-type w/ ignored R2?)
      m5_op5(01100, R, OP)
      m5_op5(01101, U, LUI)
      m5_op5(01110, R, OP_32)
      m5_op5(01111, _, 64B)
      m5_op5(10000, R4, MADD)
      m5_op5(10001, R4, MSUB)
      m5_op5(10010, R4, NMSUB)
      m5_op5(10011, R4, NMADD)
      m5_op5(10100, R, OP_FP)  // (R-type, but rs2 = const for some, based on funct7 which doesn't exist for I-type?? R-type w/ ignored R2?)
      m5_op5(10101, _, RESERVED_1)
      m5_op5(10110, _, CUSTOM_2_RV128)
      m5_op5(10111, _, 48B2)
      m5_op5(11000, B, BRANCH)
      m5_op5(11001, I, JALR)
      m5_op5(11010, _, RESERVED_2)
      m5_op5(11011, J, JAL)
      m5_op5(11100, I, SYSTEM)
      m5_op5(11101, _, RESERVED_3)
      m5_op5(11110, _, CUSTOM_3_RV128)
      m5_op5(11111, _, 80B)

      // For each instruction type, a mask, where each bit indicates whether
      // the op5 corresponding to the bit position is of the given type.
      // m5_instr calls produce localparam definitions for \SV_plus context)
      m4_out_nl(\SV_plus)
      m4_out(m5_instr_types_sv(m5_instr_types_args))

      m4_out_nl(['// Instruction characterization.'])
      m4_out_nl(['// (User ISA Manual 2.2, Table 19.2)'])
      m4_out_nl(\SV_plus)
      // instr(type,  // (this is simply verified vs. op5)
      //       |  bit-width,
      //       |  |   extension, 
      //       |  |   |  opcode[6:2],  // (aka op5)
      //       |  |   |  |      func3,   // (if applicable)
      //       |  |   |  |      |    mnemonic
      m5_instr(U, 32, I, 01101,      LUI)
      m5_instr(U, 32, I, 00101,      AUIPC)
      m5_instr(J, 32, I, 11011,      JAL)
      m5_instr(I, 32, I, 11001, 000, JALR)
      m5_instr(B, 32, I, 11000, 000, BEQ)
      m5_instr(B, 32, I, 11000, 001, BNE)
      m5_instr(B, 32, I, 11000, 100, BLT)
      m5_instr(B, 32, I, 11000, 101, BGE)
      m5_instr(B, 32, I, 11000, 110, BLTU)
      m5_instr(B, 32, I, 11000, 111, BGEU)
      m5_instr(I, 32, I, 00000, 000, LB)
      m5_instr(I, 32, I, 00000, 001, LH)
      m5_instr(I, 32, I, 00000, 010, LW)
      m5_instr(I, 32, I, 00000, 100, LBU)
      m5_instr(I, 32, I, 00000, 101, LHU)
      m5_instr(S, 32, I, 01000, 000, SB)
      m5_instr(S, 32, I, 01000, 001, SH)
      m5_instr(S, 32, I, 01000, 010, SW)
      m5_instr(I, 32, I, 00100, 000, ADDI)
      m5_instr(I, 32, I, 00100, 010, SLTI)
      m5_instr(I, 32, I, 00100, 011, SLTIU)
      m5_instr(I, 32, I, 00100, 100, XORI)
      m5_instr(I, 32, I, 00100, 110, ORI)
      m5_instr(I, 32, I, 00100, 111, ANDI)
      m5_instr(If, 32, I, 00100, 001, 000000, SLLI)
      m5_instr(If, 32, I, 00100, 101, 000000, SRLI)
      m5_instr(If, 32, I, 00100, 101, 010000, SRAI)
      m5_instr(R, 32, I, 01100, 000, 0000000, ADD)
      m5_instr(R, 32, I, 01100, 000, 0100000, SUB)
      m5_instr(R, 32, I, 01100, 001, 0000000, SLL)
      m5_instr(R, 32, I, 01100, 010, 0000000, SLT)
      m5_instr(R, 32, I, 01100, 011, 0000000, SLTU)
      m5_instr(R, 32, I, 01100, 100, 0000000, XOR)
      m5_instr(R, 32, I, 01100, 101, 0000000, SRL)
      m5_instr(R, 32, I, 01100, 101, 0100000, SRA)
      m5_instr(R, 32, I, 01100, 110, 0000000, OR)
      m5_instr(R, 32, I, 01100, 111, 0000000, AND)
      //m5_instr(_, 32, I, 00011, 000, FENCE)
      //m5_instr(_, 32, I, 00011, 001, FENCE_I)
      //m5_instr(_, 32, I, 11100, 000, ECALL_EBREAK)  // Two instructions distinguished by an immediate bit, treated as a single instruction.
      m5_instr(I, 32, I, 11100, 001, CSRRW)
      m5_instr(I, 32, I, 11100, 010, CSRRS)
      m5_instr(I, 32, I, 11100, 011, CSRRC)
      m5_instr(I, 32, I, 11100, 101, CSRRWI)
      m5_instr(I, 32, I, 11100, 110, CSRRSI)
      m5_instr(I, 32, I, 11100, 111, CSRRCI)
      m5_instr(I, 64, I, 00000, 110, LWU)
      m5_instr(I, 64, I, 00000, 011, LD)
      m5_instr(S, 64, I, 01000, 011, SD)
      m5_instr(If, 64, I, 00100, 001, 000000, SLLI)
      m5_instr(If, 64, I, 00100, 101, 000000, SRLI)
      m5_instr(If, 64, I, 00100, 101, 010000, SRAI)
      m5_instr(I, 64, I, 00110, 000, ADDIW)
      m5_instr(If, 64, I, 00110, 001, 000000, SLLIW)
      m5_instr(If, 64, I, 00110, 101, 000000, SRLIW)
      m5_instr(If, 64, I, 00110, 101, 010000, SRAIW)
      m5_instr(R, 64, I, 01110, 000, 0000000, ADDW)
      m5_instr(R, 64, I, 01110, 000, 0100000, SUBW)
      m5_instr(R, 64, I, 01110, 001, 0000000, SLLW)
      m5_instr(R, 64, I, 01110, 101, 0000000, SRLW)
      m5_instr(R, 64, I, 01110, 101, 0100000, SRAW)
      m5_instr(R, 32, M, 01100, 000, 0000001, MUL)
      m5_instr(R, 32, M, 01100, 001, 0000001, MULH)
      m5_instr(R, 32, M, 01100, 010, 0000001, MULHSU)
      m5_instr(R, 32, M, 01100, 011, 0000001, MULHU)
      m5_instr(R, 32, M, 01100, 100, 0000001, DIV)
      m5_instr(R, 32, M, 01100, 101, 0000001, DIVU)
      m5_instr(R, 32, M, 01100, 110, 0000001, REM)
      m5_instr(R, 32, M, 01100, 111, 0000001, REMU)
      m5_instr(R, 64, M, 01110, 000, 0000001, MULW)
      m5_instr(R, 64, M, 01110, 100, 0000001, DIVW)
      m5_instr(R, 64, M, 01110, 101, 0000001, DIVUW)
      m5_instr(R, 64, M, 01110, 110, 0000001, REMW)
      m5_instr(R, 64, M, 01110, 111, 0000001, REMUW)
      m5_instr(I, 32, F, 00001, 010, FLW)
      m5_instr(S, 32, F, 01001, 010, FSW)
      m5_instr(R4, 32, F, 10000, rm, 00, FMADDS)
      m5_instr(R4, 32, F, 10001, rm, 00, FMSUBS)
      m5_instr(R4, 32, F, 10010, rm, 00, FNMSUBS)
      m5_instr(R4, 32, F, 10011, rm, 00, FNMADDS)
      m5_instr(R, 32, F, 10100, rm, 0000000, FADDS)
      m5_instr(R, 32, F, 10100, rm, 0000100, FSUBS)
      m5_instr(R, 32, F, 10100, rm, 0001000, FMULS)
      m5_instr(R, 32, F, 10100, rm, 0001100, FDIVS)
      m5_instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRTS)
      m5_instr(R, 32, F, 10100, 000, 0010000, FSGNJS)
      m5_instr(R, 32, F, 10100, 001, 0010000, FSGNJNS)
      m5_instr(R, 32, F, 10100, 010, 0010000, FSGNJXS)
      m5_instr(R, 32, F, 10100, 000, 0010100, FMINS)
      m5_instr(R, 32, F, 10100, 001, 0010100, FMAXS)
      m5_instr(R2, 32, F, 10100, rm, 1100000, 00000, FCVTWS)
      m5_instr(R2, 32, F, 10100, rm, 1100000, 00001, FCVTWUS)
      m5_instr(R2, 32, F, 10100, 000, 1110000, 00000, FMVXW)
      m5_instr(R, 32, F, 10100, 010, 1010000, FEQS)
      m5_instr(R, 32, F, 10100, 001, 1010000, FLTS)
      m5_instr(R, 32, F, 10100, 000, 1010000, FLES)
      m5_instr(R2, 32, F, 10100, 001, 1110000, 00000, FCLASSS)
      m5_instr(R2, 32, F, 10100, rm, 1101000, 00000, FCVTSW)
      m5_instr(R2, 32, F, 10100, rm, 1101000, 00001, FCVTSWU)
      m5_instr(R2, 32, F, 10100, 000, 1111000, 00000, FMVWX)
      m5_instr(R2, 64, F, 10100, rm, 1100000, 00010, FCVTLS)
      m5_instr(R2, 64, F, 10100, rm, 1100000, 00011, FCVTLUS)
      m5_instr(R2, 64, F, 10100, rm, 1101000, 00010, FCVTSL)
      m5_instr(R2, 64, F, 10100, rm, 1101000, 00011, FCVTSLU)
      m5_instr(I, 32, D, 00001, 011, FLD)
      m5_instr(S, 32, D, 01001, 011, FSD)
      m5_instr(R4, 32, D, 10000, rm, 01, FMADDD)
      m5_instr(R4, 32, D, 10001, rm, 01, FMSUBD)
      m5_instr(R4, 32, D, 10010, rm, 01, FNMSUBD)
      m5_instr(R4, 32, D, 10011, rm, 01, FNMADDD)
      m5_instr(R, 32, D, 10100, rm, 0000001, FADDD)
      m5_instr(R, 32, D, 10100, rm, 0000101, FSUBD)
      m5_instr(R, 32, D, 10100, rm, 0001001, FMULD)
      m5_instr(R, 32, D, 10100, rm, 0001101, FDIVD)
      m5_instr(R2, 32, D, 10100, rm, 0101101, 00000, FSQRTD)
      m5_instr(R, 32, D, 10100, 000, 0010001, FSGNJD)
      m5_instr(R, 32, D, 10100, 001, 0010001, FSGNJND)
      m5_instr(R, 32, D, 10100, 010, 0010001, FSGNJXD)
      m5_instr(R, 32, D, 10100, 000, 0010101, FMIND)
      m5_instr(R, 32, D, 10100, 001, 0010101, FMAXD)
      m5_instr(R2, 32, D, 10100, rm, 0100000, 00001, FCVTSD)
      m5_instr(R2, 32, D, 10100, rm, 0100001, 00000, FCVTDS)
      m5_instr(R, 32, D, 10100, 010, 1010001, FEQD)
      m5_instr(R, 32, D, 10100, 001, 1010001, FLTD)
      m5_instr(R, 32, D, 10100, 000, 1010001, FLED)
      m5_instr(R2, 32, D, 10100, 001, 1110001, 00000, FCLASSD)
      m5_instr(R2, 32, D, 10100, rm, 1110001, 00000, FCVTWD)
      m5_instr(R2, 32, D, 10100, rm, 1100001, 00001, FCVTWUD)
      m5_instr(R2, 32, D, 10100, rm, 1101001, 00000, FCVTDW)
      m5_instr(R2, 32, D, 10100, rm, 1101001, 00001, FCVTDWU)
      m5_instr(R2, 64, D, 10100, rm, 1100001, 00010, FCVTLD)
      m5_instr(R2, 64, D, 10100, rm, 1100001, 00011, FCVTLUD)
      m5_instr(R2, 64, D, 10100, 000, 1110001, 00000, FMVXD)
      m5_instr(R2, 64, D, 10100, rm, 1101001, 00010, FCVTDL)
      m5_instr(R2, 64, D, 10100, rm, 1101001, 00011, FCVTDLU)
      m5_instr(R2, 64, D, 10100, 000, 1111001, 00000, FMVDX)
      m5_instr(I, 32, Q, 00001, 100, FLQ)
      m5_instr(S, 32, Q, 01001, 100, FSQ)
      m5_instr(R4, 32, Q, 10000, rm, 11, FMADDQ)
      m5_instr(R4, 32, Q, 10001, rm, 11, FMSUBQ)
      m5_instr(R4, 32, Q, 10010, rm, 11, FNMSUBQ)
      m5_instr(R4, 32, Q, 10011, rm, 11, FNMADDQ)
      m5_instr(R, 32, Q, 10100, rm, 0000011, FADDQ)
      m5_instr(R, 32, Q, 10100, rm, 0000111, FSUBQ)
      m5_instr(R, 32, Q, 10100, rm, 0001011, FMULQ)
      m5_instr(R, 32, Q, 10100, rm, 0001111, FDIVQ)
      m5_instr(R2, 32, Q, 10100, rm, 0101111, 00000, FSQRTQ)
      m5_instr(R, 32, Q, 10100, 000, 0010011, FSGNJQ)
      m5_instr(R, 32, Q, 10100, 001, 0010011, FSGNJNQ)
      m5_instr(R, 32, Q, 10100, 010, 0010011, FSGNJXQ)
      m5_instr(R, 32, Q, 10100, 000, 0010111, FMINQ)
      m5_instr(R, 32, Q, 10100, 001, 0010111, FMAXQ)
      m5_instr(R2, 32, Q, 10100, rm, 0100000, 00011, FCVTSQ)
      m5_instr(R2, 32, Q, 10100, rm, 0100011, 00000, FCVTQS)
      m5_instr(R2, 32, Q, 10100, rm, 0100001, 00011, FCVTDQ)
      m5_instr(R2, 32, Q, 10100, rm, 0100011, 00001, FCVTQD)
      m5_instr(R, 32, Q, 10100, 010, 1010011, FEQQ)
      m5_instr(R, 32, Q, 10100, 001, 1010011, FLTQ)
      m5_instr(R, 32, Q, 10100, 000, 1010011, FLEQ)
      m5_instr(R2, 32, Q, 10100, 001, 1110011, 00000, FCLASSQ)
      m5_instr(R2, 32, Q, 10100, rm, 1110011, 00000, FCVTWQ)
      m5_instr(R2, 32, Q, 10100, rm, 1100011, 00001, FCVTWUQ)
      m5_instr(R2, 32, Q, 10100, rm, 1101011, 00000, FCVTQW)
      m5_instr(R2, 32, Q, 10100, rm, 1101011, 00001, FCVTQWU)
      m5_instr(R2, 64, Q, 10100, rm, 1100011, 00010, FCVTLQ)
      m5_instr(R2, 64, Q, 10100, rm, 1100011, 00011, FCVTLUQ)
      m5_instr(R2, 64, Q, 10100, rm, 1101011, 00010, FCVTQL)
      m5_instr(R2, 64, Q, 10100, rm, 1101011, 00011, FCVTQLU)
      m5_instr(R2, 32, A, 01011, 010, 00010, 00000, LRW)
      m5_instr(R, 32, A, 01011, 010, 00011, SCW)
      m5_instr(R, 32, A, 01011, 010, 00001, AMOSWAPW)
      m5_instr(R, 32, A, 01011, 010, 00000, AMOADDW)
      m5_instr(R, 32, A, 01011, 010, 00100, AMOXORW)
      m5_instr(R, 32, A, 01011, 010, 01100, AMOANDW)
      m5_instr(R, 32, A, 01011, 010, 01000, AMOORW)
      m5_instr(R, 32, A, 01011, 010, 10000, AMOMINW)
      m5_instr(R, 32, A, 01011, 010, 10100, AMOMAXW)
      m5_instr(R, 32, A, 01011, 010, 11000, AMOMINUW)
      m5_instr(R, 32, A, 01011, 010, 11100, AMOMAXUW)
      m5_instr(R2, 64, A, 01011, 011, 00010, 00000, LRD)
      m5_instr(R, 64, A, 01011, 011, 00011, SCD)
      m5_instr(R, 64, A, 01011, 011, 00001, AMOSWAPD)
      m5_instr(R, 64, A, 01011, 011, 00000, AMOADDD)
      m5_instr(R, 64, A, 01011, 011, 00100, AMOXORD)
      m5_instr(R, 64, A, 01011, 011, 01100, AMOANDD)
      m5_instr(R, 64, A, 01011, 011, 01000, AMOORD)
      m5_instr(R, 64, A, 01011, 011, 10000, AMOMIND)
      m5_instr(R, 64, A, 01011, 011, 10100, AMOMAXD)
      m5_instr(R, 64, A, 01011, 011, 11000, AMOMINUD)
      m5_instr(R, 64, A, 01011, 011, 11100, AMOMAXUD)
      m5_instr(R, 32, B, 01100, 111, 0100000, ANDN)
      m5_instr(R, 32, B, 01100, 110, 0100000, ORN)
      m5_instr(R, 32, B, 01100, 100, 0100000, XNOR)
      m5_instr(R, 32, B, 01100, 001, 0010000, SLO)
      m5_instr(R, 32, B, 01100, 101, 0010000, SRO)
      m5_instr(R, 32, B, 01100, 001, 0110000, ROL)
      m5_instr(R, 32, B, 01100, 101, 0110000, ROR)
      m5_instr(R, 32, B, 01100, 001, 0100100, SBCLR)
      m5_instr(R, 32, B, 01100, 001, 0010100, SBSET)
      m5_instr(R, 32, B, 01100, 001, 0110100, SBINV)
      m5_instr(R, 32, B, 01100, 101, 0100100, SBEXT)
      m5_instr(R, 32, B, 01100, 101, 0010100, GORC)
      m5_instr(R, 32, B, 01100, 101, 0110100, GREV)
      m5_instr(If, 32, B, 00100, 001, 00100, SLOI)
      m5_instr(If, 32, B, 00100, 101, 00100, SROI)
      m5_instr(If, 32, B, 00100, 101, 01100, RORI)
      m5_instr(If, 32, B, 00100, 001, 01001, SBCLRI)
      m5_instr(If, 32, B, 00100, 001, 00101, SBSETI)
      m5_instr(If, 32, B, 00100, 001, 01101, SBINVI)
      m5_instr(If, 32, B, 00100, 101, 01001, SBEXTI)
      m5_instr(If, 32, B, 00100, 101, 00101, GORCI)
      m5_instr(If, 32, B, 00100, 101, 01101, GREVI)
      //m5_instr(R4, 32, B, 01100, 001, 11, CMIX)  // Currently we are excluing ternary BMI's for optimization, although its only a draft
      //m5_instr(R4, 32, B, 01100, 101, 11, CMOV)
      //m5_instr(R4, 32, B, 01100, 001, 10, FSL)
      //m5_instr(R4, 32, B, 01100, 101, 10, FSR)
      //m5_instr(R4, 32, B, 00100, 101, 10, FSRI)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 00000, CLZ) // single operand instruction are currently using R2-type rather than I-type encoding
      m5_instr(R2, 32, B, 01100, 001, 0110000, 00001, CTZ)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 00010, PCNT)
      //m5_instr(R2, 64, B, 01100, 001, 0110000, 00011, BMATFLIP)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 00100, SEXTB)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 00101, SEXTH)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 10000, CRC32B)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 10001, CRC32H)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 10010, CRC32W)
      //m5_instr(R2, 64, B, 01100, 001, 0110000, 10011, CRC32D)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 11000, CRC32CB)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 11001, CRC32CH)
      m5_instr(R2, 32, B, 01100, 001, 0110000, 11010, CRC32CW)
      //m5_instr(R2, 64, B, 01100, 001, 0110000, 11011, CRC32CD)
      m5_instr(R, 32, B, 01100, 001, 0000101, CLMUL)
      m5_instr(R, 32, B, 01100, 010, 0000101, CLMULR)
      m5_instr(R, 32, B, 01100, 011, 0000101, CLMULH)
      m5_instr(R, 32, B, 01100, 100, 0000101, MIN)
      m5_instr(R, 32, B, 01100, 101, 0000101, MAX)
      m5_instr(R, 32, B, 01100, 110, 0000101, MINU)
      m5_instr(R, 32, B, 01100, 111, 0000101, MAXU)
      m5_instr(R, 32, B, 01100, 001, 0000100, SHFL)
      m5_instr(R, 32, B, 01100, 101, 0000100, UNSHFL)
      m5_instr(R, 32, B, 01100, 110, 0100100, BDEP)
      m5_instr(R, 32, B, 01100, 110, 0000100, BEXT)
      m5_instr(R, 32, B, 01100, 100, 0000100, PACK)
      m5_instr(R, 32, B, 01100, 100, 0100100, PACKU)
      //m5_instr(R, 64, B, 01100, 011, 0000100, BMATOR)
      //m5_instr(R, 64, B, 01100, 011, 0100100, BMATXOR)
      m5_instr(R, 32, B, 01100, 111, 0000100, PACKH)
      m5_instr(R, 32, B, 01100, 111, 0100100, BFP)
      m5_instr(If, 32, B, 00100, 001, 000010, SHFLI)
      m5_instr(If, 32, B, 00100, 101, 000010, UNSHFLI)
      //m5_instr(I, 64, B, 01100, 100, ADDIWU)
      //m5_instr(If, 64, B, 01100, 001, 000010, SLLIUW)
      //m5_instr(R, 64, B, 01110, 000, 0000101, ADDWU)
      //m5_instr(R, 64, B, 01110, 000, 0100101, SUBWU)
      //m5_instr(R, 64, B, 01110, 000, 0000100, ADDUW)
      //m5_instr(R, 64, B, 01110, 000, 0100100, SUBUW)
      //m5_instr(R, 64, B, 01110, 001, 0010000, SLOW)
      //m5_instr(R, 64, B, 01110, 101, 0010000, SROW)
      //m5_instr(R, 64, B, 01110, 001, 0110000, ROLW)
      //m5_instr(R, 64, B, 01110, 101, 0110000, RORW)
      //m5_instr(R, 64, B, 01110, 001, 0100100, SBCLRW)
      //m5_instr(R, 64, B, 01110, 001, 0010100, SBSETW)
      //m5_instr(R, 64, B, 01110, 001, 0110100, SBINVW)
      //m5_instr(R, 64, B, 01110, 101, 0100100, SBEXTW)
      //m5_instr(R, 64, B, 01110, 101, 0010100, GROCW)
      //m5_instr(R, 64, B, 01110, 101, 0110100, GREVW)
      //m5_instr(If, 64, B, 00110, 001, 001000, SLOIW)
      //m5_instr(If, 64, B, 00110, 101, 001000, SROIW)
      //m5_instr(If, 64, B, 00110, 101, 011000, RORIW)
      //m5_instr(If, 64, B, 00110, 001, 010010, SBCLRIW)
      //m5_instr(If, 64, B, 00110, 001, 001010, SBSETIW)
      //m5_instr(If, 64, B, 00110, 001, 011010, SBINVIW)
      //m5_instr(If, 64, B, 00110, 101, 001010, GORCIW)
      //m5_instr(If, 64, B, 00110, 101, 011010, GREVIW)
      //m5_instr(R4, 64, B, 01110, 001, 10, FSLW)
      //m5_instr(R4, 64, B, 01110, 101, 10, FSRW)
      //m5_instr(R4, 64, B, 00110, 101, 10, FSRIW) //rs2 is "imm"
      //m5_instr(R2, 64, B, 00110, 001, 0110000, 00000, CLZW)
      //m5_instr(R2, 64, B, 00110, 001, 0110000, 00001, CTZW)
      //m5_instr(R2, 64, B, 00110, 001, 0110000, 00010, PCNTW)
      //m5_instr(R, 64, B, 01110, 001, 0000101, CLMULW)
      //m5_instr(R, 64, B, 01110, 010, 0000101, CLMULRW)
      //m5_instr(R, 64, B, 01110, 011, 0000101, CLMULHW)
      //m5_instr(R, 64, B, 01110, 001, 0000100, SHFLW)
      //m5_instr(R, 64, B, 01110, 101, 0000100, UNSHFLW)
      //m5_instr(R, 64, B, 01110, 110, 0100100, BDEPW)
      //m5_instr(R, 64, B, 01110, 110, 0000100, BEXTW)
      //m5_instr(R, 64, B, 01110, 100, 0000100, PACKW)
      //m5_instr(R, 64, B, 01110, 100, 0100100, PACKUW)
      //m5_instr(R, 64, B, 01110, 111, 0100100, BFPW)
   '])
\TLV
   
