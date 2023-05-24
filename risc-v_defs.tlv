\m5_TLV_version 1d: tl-x.org
\m5
  /Instruction field values for each instruction are defined as localparams and as M4 defines. Assembly uses one or the other
  /depending on m5_use_localparams.
  /Define localparam and m5_ constant.
  /m5_define_localparam(<name>, <localparam-bit-range>, <value>)
  /Backward compatibility, convert macro definition to var.
  if_def(use_localparams, ['m5_var(['use_localparams'], m5_use_localparams)'])
  default_var(use_localparams, 0)
  macro(define_localparam,
      ['m5_universal_var(['$1'], ['$3'])m5_if(m5_use_localparams, ['['localparam $2 $1 = $3;']'])'])
  /Use defined localparam or m5_ constant, depending on m5_use_localparams.
  macro(localparam_value,
      ['m5_if(m5_use_localparams, ['$1'], ['m5_get($1)'])'])

  /--------------------------------------
  /Associate each op5 value with an instruction type.
  /--------------------------------------

  /TODO:
  /We construct m5_OP5_XXXXX_TYPE, and verify each instruction against that.
  /Instruction fields are constructed and valid based on op5.
  /...
  /TO UPDATE:
  /We construct localparam INSTR_TYPE_X_MASK as a mask, one bit per op5 indicating whether the op5 is of the type.
  /Instantiated recursively for each instruction type.
  /Initializes m5_instr_type_X_mask_expr which will build up a mask, one bit per op5.
  macro(instr_types,
      ['m5_if_eq($1, [''], [''],
                 ['m5_universal_var(instr_type_$1_mask_expr, 0)m5_instr_types(m5_shift($@))'])'])
  /Instantiated recursively for each instruction type in \SV_plus context after characterizing each type.
  /Declares localparam INSTR_TYPE_X_MASK as m5_instr_type_X_mask_expr.
  fn(instr_types_sv, ..., [
     ~if_eq(['$1'], [''], [''], [
        ~(['    ']m5_define_localparam(['INSTR_TYPE_$1_MASK'], ['[31:0]'], m5_get(instr_type_$1_mask_expr))m5_nl)
        ~instr_types_sv(m5_shift($@))
     ])
  ])
  /Instantiated recursively for each instruction type in \SV_plus context to decode instruction type.
  /Creates "assign $$is_x_type = INSTR_TYPE_X_MASK[$raw_op5];" for each type.
  /TODO: Not sure how to extract a bit ($raw_op) from a constant expression. Hoping synthesis optimizes well.
  macro(types_decode,
         ['m5_if_eq(['$1'], [''], [''],
                    ['m5_nl['   assign $$is_']m5_translit_eval(['$1'], ['A-Z'], ['a-z'])['_type = (((']m5_localparam_value(['INSTR_TYPE_$1_MASK'])[') >> $raw_op5) & 32'b1) != 32'b0; ']m5_types_decode(m5_shift($@))'])'])
  /Instantiated for each op5 in \SV_plus context.
  fn(op5, ..., [
     universal_var(OP5_$1_TYPE, $2)
     universal_var(op5_named_$3, $1)
     ~nl(['   ']m5_define_localparam(['OP5_$3'], ['[4:0]'], ['5'b$1']))
     set(instr_type_$2_mask_expr, m5_get(instr_type_$2_mask_expr)[' | (1 << 5'b$1)'])
  ])


  /--------------------------------
  /Characterize each instruction mnemonic
  /--------------------------------

  /Each instruction is defined by instantiating m5_instr(...), e.g.: 
  /   m5_instr(B, 32, I, 11000, 000, BEQ)
  /which instantiates an instruction-type-specific macro, e.g.:
  /   m5_instrB(32, I, 11000, 000, BEQ)
  /which produces (or defines macros for):
  /  o instruction decode logic ($is_<mnemonic>_instr = ...;)
  /  o for debug, an expression to produce the MNEMONIC.
  /  o result MUX expression to select result of the appropriate execution expression
  /  o $illegal_instruction expression
  /  o localparam definitions for fields
  /  o m5_asm(<MNEMONIC>, ...) to assemble instructions to their binary representations

  /Return 1 if the given instruction is supported, [''] otherwise.
  /m5_instr_supported(<args-of-m5_instr(...)>)
  macro(instr_supported,
      ['m5_if_eq(m5_get(EXT_$3), 1,
                 ['m5_if_eq(m5_WORD_CNT, ['$2'], 1, [''])'])'])

  /Called for each instruction.
  /Outputs a string to evaluate that outputs indented content for \SV_plus context.
  /m5_instr(<instr-type-char(s)>, <type-specific-args>)
  /This instantiates m5_instr<type>(type-specific-args>)
  fn(instr, [1]type, [2]width, [3]ext, [4]op5, ..., [
     var(mnemonic, m5_argn($#, $@))
     /Define the instruction -> op5 map.
     universal_var(['op5_of_instr_']m5_mnemonic, m5_op5)
     /check instr type
     if_neq(m5_get(OP5_$4_TYPE), m5_if_var_def(instr_type_of_$1, ['m5_get(instr_type_of_$1)'], ['$1']),
        ['m5_error(['Instruction ']m5_mnemonic[''s type (']m5_type[') is inconsistant with its op5 code (']m5_op5[') of type ']m5_get(['OP5_']m5_op5['_TYPE'])['.'] m5_get(OP5_$4_TYPE) m5_if_var_def(instr_type_of_$1, ['m5_get(instr_type_of_$1)'], ['$1']) )'])
     /if instrs extension is supported and instr is for the right machine width, include it
     ~if_eq(m5_instr_supported($@), 1, [
        universal_var(['instr_defined_']m5_mnemonic, ['yes'])
        ~(['   ']m5_define_localparam(m5_mnemonic['_INSTR_OPCODE'], ['[6:0]'], ['7'b']m5_op5['11']))
        ~(m5_call(instr$1, m5_mnemonic, m5_shift($@))m5_nl)
     ])
  ])


  /Decode logic for instructions with various opcode/func bits that dictate the mnemonic.
  /(This would be easier if we could use 'x', but Yosys doesn't support ==?/!=? operators.)
  /Helpers to deal with "rm" cases:
  macro(op5_and_funct3,
     ['$raw_op5 == 5'b$3 m5_if_eq($4, ['rm'], [''], ['&& $raw_funct3 == 3'b$4'])'])
  macro(funct3_localparam,
     ['m5_if_eq(['$2'], ['rm'], [''], ['m5_define_localparam(['$1_INSTR_FUNCT3'], ['[2:0]'], ['3'b$2'])'])'])
  /m5_asm_<MNEMONIC> output for funct3 or rm, returned in unquoted context so arg references can be produced. 'rm' is always the last m5_asm_<MNEMONIC> arg.
  /  Args: $1: MNEMONIC, $2: funct3 field of instruction definition (or 'rm')
  /TODO: Remove "new_" from name below.
  macro(asm_funct3, ['['m5_if_eq($2, ['rm'], ['3'b$3'], m5_localparam_value(['$1_INSTR_FUNCT3']))']'])
  
  /Opcode + funct3 + funct7 (R-type, R2-type). $@ as for m5_instrX(..), $7: MNEMONIC, $8: number of bits of leading bits of funct7 to interpret. If 5, for example, use the term funct5, $9: (opt) for R2, the r2 value.
  macro(instr_funct7,
     ['m5_instr_decode_expr($7, m5_op5_and_funct3($@)[' && $raw_funct7'][6:m5_calc(7-$8)][' == $8'b$5']m5_if_eq($9, [''], [''], [' && $raw_rs2 == 5'b$9']))m5_funct3_localparam(['$7'], ['$4'])m5_define_localparam(['$7_INSTR_FUNCT$8'], ['[$8-1:0]'], ['$8'b$5'])'])
  /For cases w/ extra shamt bit that cuts into funct7.
  macro(instr_funct6,
     ['m5_instr_decode_expr($7, m5_op5_and_funct3($@)[' && $raw_funct7[6:1] == 6'b$5'])m5_funct3_localparam(['$7'], ['$4'])m5_define_localparam(['$7_INSTR_FUNCT6'], ['[6:0]'], ['6'b$5'])'])
  /Opcode + funct3 + func7[1:0] (R4-type)
  macro(instr_funct2,
         ['m5_instr_decode_expr($6, m5_op5_and_funct3($@)[' && $raw_funct7[1:0] == 2'b$5'])m5_funct3_localparam(['$6'], ['$4'])m5_define_localparam(['$6_INSTR_FUNCT2'], ['[1:0]'], ['2'b$5'])'])
  /Opcode + funct3 + funct7[6:2] (R-type where funct7 has two lower bits that do not distinguish mnemonic.)
  macro(instr_funct5,
         ['m5_instr_decode_expr($6, m5_op5_and_funct3($@)[' && $raw_funct7[6:2] == 5'b$5'])m5_funct3_localparam(['$6'], ['$4'])m5_define_localparam(['$6_INSTR_FUNCT5'], ['[4:0]'], ['5'b$5'])'])
  /Opcode + funct3
  macro(instr_funct3,
         ['m5_instr_decode_expr($5, m5_op5_and_funct3($@), $6)m5_funct3_localparam(['$5'], ['$4'])'])
  /Opcode
  fn(instr_no_func, mnemonic, op5, [
     instr_decode_expr(m5_mnemonic, ['$raw_op5 == 5'b']m5_op5)
  ])
  macro(instr_viz,
     ['m5_if_eq(['$1'], [''], [''],
                ['['is_instr("$1", '$is_']['$1']['_instr'); ']$0(m5_shift($@))'])'])
  /m5_instr_decode_expr(<mnemonic>, <decode_expr>, (opt)['no_dest']/other)
  /Extends the following definitions to reflect the given instruction <mnemonic>:
  universal_vars(
      decode_expr, [''],        /// instructiton decode: $is_<mnemonic>_instr = ...; ...
      rslt_mux_expr, [''],      /// result combining expr.: ({32{$is_[mnemonic]_instr}} & $[mnemonic]_rslt) | ...
      illegal_instr_expr, [''], /// $illegal instruction exception expr: && ! $is_<mnemonic>_instr ...
      mnemonic_expr, [''])      /// $is_<mnemonic>_instr ? "<MNEMONIC>" : ...
  fn(instr_decode_expr, Mnemonic, Expr, ?Type, {
     /Lower case instruction name
     var(lc_name, m5_translit_eval(m5_Mnemonic, ['A-Z'], ['a-z']))
     append_var(decode_expr, ['$is_']m5_lc_name['_instr = ']m5_Expr[';']m5_nl)
     if_neq(m5_Type, ['no_dest'], [
        append_var(rslt_mux_expr, [' |']['m4_plus_new_line       ({']m5_WORD_CNT['{$is_']m5_lc_name['_instr}} & $']m5_lc_name['_rslt)'])
     ])
     append_var(illegal_instr_expr, [' && ! $is_']m5_lc_name['_instr'])
     append_var(mnemonic_expr, ['$is_']m5_lc_name['_instr ? "']m5_Mnemonic\m5_substr_eval(['          '], m5_length(m5_Mnemonic))['" : '])
  })

  /The first arg of m5_instr(..) is a type, and a type-specific macro is invoked. Types are those defined by RISC-V, plus:
  /  R2: R-type with a hard-coded rs2 value. (assuming illegal instruction exception should be thrown for wrong value--not clear in RISC-V spec)
  /  If: I-type with leading bits of imm[11:...] used as function bits.

  vars(instr_type_of_R2, R,
       instr_type_of_If, I)
  /Unique to each instruction type, eg:
  /  ~instr(U, 32, I, 01101,      LUI)
  /  ~instr(J, 32, I, 11011,      JAL)
  /  ~instr(B, 32, I, 11000, 000, BEQ)
  /  ~instr(S, 32, I, 01000, 000, SB)
  /  ~instr(I, 32, I, 00100, 000, ADDI)
  /  ~instr(If, 64, I, 00100, 101, 000000, SRLI)  /// (imm[11:6] are used like funct7[6:1] and must be 000000)
  /  ~instr(R, 32, I, 01100, 000, 0000000, ADD)
  /  ~instr(R4, 32, F, 10000, rm, 10, FMADD.D)
  /  ~instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRT.S)
  /  ~instr(R2, 32, A, 01011, 010, 00010, 00000, LR.W)  /// (5 bits for funct7 for all "A"-ext instrs)
  /  ~instr(R, 32, A, 01011, 010, 00011, SC.W)          ///   "
  /This defines assembler macros as follows. Fields are ordered rd, rs1, rs2, imm:
  /  I:  m5_asm_ADDI(r4, r1, 0),
  /  R:  m5_asm_ADD(r4, r1, r2),
  /  R2: m5_asm_FSQRT.S(r4, r1, 000),  /// rm == 000
  /  R4: m5_asm_FMADD.S(r4, r1, r2, r3, 000),  /// rm == 000
  /  S:  m5_asm_SW(r1, r2, 100),  /// Store r13 into [r10] + 4
  /  J:  m5_asm_JAL(r7, 000000000000000000010),  /// Jump to next instr. Up to 21-bit signed immediate including 0 representing implicit bit-0 (unlike JALR) provides byte address
  /  B:  m5_asm_BLT(r1, r2, 1000), /// Branch if r1 < r2 to PC + 13'b1000 (where lsb = 0)
  /  For "A"-extension instructions, an additional final arg is REQUIRED to provide 2 binary bits for aq and rl.
  /Macro definitions include 2 parts:
  /  o Hardware definitions: m5_instr_<mnemonic>($@)
  /  o Assembler definition of m5_asm_<MNEMONIC>: m5_def(['asm_<MNEMONIC>'], ['m5_asm_instr_str(...)'])
  fn(instrI, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^mnemonic, {
        asm_instr_str(I, m5_mnemonic, m5_fn_args())
        /TODO: This isn't working with imm vs. imm2. Maybe because function isn't properly using vars for parameters?
        var(imm2, m5_immediate_field_to_bits(12, m5_imm))
        ~quote(['{12'b']m5_imm2[', ']m5_asm_reg(m5_src1)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT3'])[', ']m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrIf, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct7($@, m5_mnemonic, m5_length(m5_imm_funct))
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^imm_funct, ^mnemonic, {
        asm_instr_str(I, m5_mnemonic, m5_fn_args())
        var(imm_width, m5_calc(12-m5_length(m5_imm_funct)))
        set(imm, m5_immediate_field_to_bits(m5_imm_width, m5_imm))
        ~quote(['{']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT']m5_length(m5_imm_funct))[', ']m5_imm_width[''b']m5_imm[', ']m5_asm_reg(m5_src1)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT3'])[', ']m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrR, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct7($@, m5_mnemonic, m5_if_eq(m5_ext, ['A'], 5, 7))
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]src2, ?rm, ^ext, ^funct3, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m5_fn_args()m5_if_null(rm, [''], [', m5_rm']))
        ~quote(['{']m5_if_eq(m5_ext, ['A'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT5'])[', ']m5_src1'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT7'])'])[', ']m5_asm_reg(m5_src2)[', ']m5_asm_reg(m5_src1)[', ']m5_asm_funct3(m5_mnemonic, m5_funct3, m5_rm)[', ']m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrR2, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, [6]fixed_src2, ..., {
     ~instr_funct7($@, 7, m5_fixed_src2)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, ?rm, ^ext, ^funct3, ^fixed_src2, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m5_fn_args()\m5_if_null(rm, [''], [', m5_rm']))
        ~quote(['{']m5_if_eq(m5_ext, ['A'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT5'])[', ']m5_src1'], ['m5_localparam_value(m5_mnemonic['_INSTR_FUNCT7'])'])[', 5'b']m5_fixed_src2[', ']m5_asm_reg(m5_src1)[', ']m5_asm_funct3(m5_mnemonic, m5_funct3, m5_rm)[', ']m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrR4, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, [5]imm_funct, ..., {
     ~instr_funct2($@)
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]src2, [4]src3, ?rm, ^funct3, ^mnemonic, {
        asm_instr_str(R, m5_mnemonic, m5_fn_args()m5_if_null(rm, [''], [', m5_rm']))
        ~quote(['{']m5_asm_reg(m5_src3)[', ']m5_localparam_value(m5_mnemonic['_INSTR_FUNCT2'])[', ']m5_asm_reg(m5_src2)[', ']m5_asm_reg(m5_src1)[', ']m5_asm_funct3(m5_mnemonic, m5_funct3, m5_rm)[', ']m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrS, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@, ['no_dest'])
     fn(['asm_']m5_mnemonic, [1]dest, [2]src1, [3]imm, ^funct3, ^mnemonic, {
        asm_instr_str(S, m5_mnemonic, m5_fn_args())
        set(imm, m5_immediate_field_to_bits(12, m5_imm))
        ~quote(['{']m5_asm_imm_field(m5_imm, 12, 11, 5)[', ']m5_asm_reg(m5_src1)[', ']m5_asm_reg(m5_dest)[', ']m5_asm_funct3(m5_mnemonic, m5_funct3)[', ']m5_asm_imm_field(m5_imm, 12, 4, 0)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrB, mnemonic, [1]width, [2]ext, [3]op5, [4]funct3, ..., {
     ~instr_funct3($@, no_dest)
     fn(['asm_']m5_mnemonic, [1]src1, [2]src2, [3]target, ^funct3, ^mnemonic, {
        var(imm, m5_asm_target(13, m5_target))
        asm_instr_str(B, m5_mnemonic, m5_fn_args())
        /m5_imm can be a label expression, so it's evaluation must be deferred, making the expressions below a bit messy.
        ~(['['{']m5_asm_imm_field(']m5_imm[', 13, 12, 12)[', ']m5_asm_imm_field(']m5_imm[', 13, 10, 5)[', ']'])
        ~quote(m5_asm_reg(m5_src2)[', ']m5_asm_reg(m5_src1)[', ']m5_asm_funct3(m5_mnemonic, m5_funct3)[', '])
        ~(['m5_asm_imm_field(']m5_imm[', 13, 4, 1)[', ']m5_asm_imm_field(']m5_imm[', 13, 11, 11)[', ']'])
        ~quote(m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrU, mnemonic, [1]width, [2]ext, [3]op5, ..., {
     ~instr_no_func(m5_mnemonic, m5_op5)
     fn(['asm_']m5_mnemonic, [1]dest, [2]imm, ^mnemonic, {
        asm_instr_str(U, m5_mnemonic, m5_fn_args())
        set(imm, m5_immediate_field_to_bits(20, m5_imm))
        ~quote(['{']m5_asm_imm_field(m5_imm, 20, 19, 0)[', ']m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instrJ, mnemonic, [1]width, [2]ext, [3]op5, ..., {
     ~instr_no_func(m5_mnemonic, m5_op5)
     fn(['asm_']m5_mnemonic, [1]dest, [2]target, ^mnemonic, {
        var(imm, m5_asm_target(21, m5_target))
        asm_instr_str(J, m5_mnemonic, m5_fn_args())
        /m5_imm can be a label expression, so it's evaluation must be deferred, making the expressions below a bit messy.
        ~(['['{']m5_asm_imm_field(']m5_imm[', 21, 20, 20)[', ']m5_asm_imm_field(']m5_imm[', 21, 10, 1)[', ']m5_asm_imm_field(']m5_imm[', 21, 11, 11)[', ']m5_asm_imm_field(']m5_imm[', 21, 19, 12)[', ']'])
        ~quote(m5_asm_reg(m5_dest)[', ']m5_localparam_value(m5_mnemonic['_INSTR_OPCODE'])['}'])
     })
  })
  fn(instr_, mnemonic, [1]width, [2]ext, [3]op5, {
     ~instr_no_func(m5_mnemonic, m5_op5)
  })

  /For each instruction type.
  /Declare localparam[31:0] INSTR_TYPE_X_MASK, initialized to 0 that will be given a 1 bit for each op5 value of its type.
  macro(instr_types_args, ['I, R, R2, R4, S, B, J, U, _'])
  instr_types(m5_instr_types_args())


  /Instruction fields (User ISA Manual 2.2, Fig. 2.2)
  /TODO: m5_define_fields doesn't exist, but this didn't break WARP-V?
  define_fields(INSTR, 32, FUNCT7, 25, RS2, 20, RS1, 15, FUNCT3, 12, RD, 7, OP5, 2, OP2, 0)

  /=========
  /Specifically for assembler.

  /For labels:
  /m5_signed_int_to_fixed_binary(digits, value)
  /Returns a string of 0s and 1s, representing the zero-padded binary value.
  /Args:
  /  digits: number of binary digits
  /  value: value (in the range -2**(digits-1) .. 2**(digits-1)-1)
  macro(signed_int_to_fixed_binary, ['m5_if(m5_calc($2 < 0), ['m5_if($2 < -(2 ** ($1 - 1)), ['m5_error(['Value $2 out of range for $1-bit signed integer.'])'])m5_calc($2 + 2 ** $1, 2, ['$1'])'], ['m5_if($2 >= 2 ** ($1 - 1), ['m5_error(['Value $2 out of range for $1-bit signed integer.'])'])m5_calc($2, 2, ['$1'])'])'])
  macro(define_label, ['m5_universal_var(label_$1_addr, $2)'])
  /m5_label_to_imm(label, bit-width, num-instrs): Convert a label (excluding :) to an immediate for current m5_NUM_INSTRS.
  macro(label_to_imm, ['m5_signed_int_to_fixed_binary($2, m5_if_var_def(['label_$1_addr'], ['m5_calc((m5_get(label_$1_addr) - ['$3']) * 4)'], ['m5_error(['No assembler label "']$1['".'])0']))'])
  
  /m5_asm_target(width, target): Output the offset for a given branch target arg of the form :label or 1111111111000, with the given bit width.
  fn(asm_target, [1], [2], {
     ~if_regex(['$2'], ['^:?\([a-zA-Z]\w*\|[0-9]+[fb]\)$'], (target), [
        /Legacy M4-style label references (starting w/ ":"), or
        /Named label target.
        /Note: The label may not have been encountered yet.
        /      This expression will be evaluated when the memory value is instantiated.
        ~(['m5_label_to_imm(']m5_target[', $1, ']m5_NUM_INSTRS[')'])
     ])
     ~else([
        ~immediate_field_to_bits(['$1'], ['$2'])
     ])
  })
  
  /Compute binary negative, given a string of 0/1 starting w/ the sign bit.
  fn(_asm_binary_negative, bin, {
     /Flip all the bits and add one.
     /Actually, adding one is flipping LSBs that match 0?1*, so just flip bits prior to 1?0*.
     var_regex(m5_bin, ['\(1?0*\)$'], (lsbs))
     ~translit(m5_substr_eval(m5_bin, 0, m5_calc(m5_length(m5_bin) - m5_length(m5_lsbs))), ['01'], ['10'])  /// Flipped bits
     ~substr_eval(m5_bin, m5_calc(m5_length(m5_bin) - m5_length(m5_lsbs)))  /// Non-flipped lsbs.
  })
  
  fn(immediate_field_to_bits, NumBits, Field, {
     /TODO: Support 0x, 0b, and decimal. ... I can't find a spec.
     /TODO: As is, this could use m5_calc for all radixes, but need to find the proper syntax anyway.
     var_regex(m5_Field, ['^\(-?\)\(0x\)\([0-9a-fA-F]+\)$'], (Sign, Base, Value))
     else([
        var_regex(m5_Field, ['^\(-?\)\(0b\)\([10]+\)$'], (Sign, Base, Value))
        else([
           var_regex(m5_Field, ['^\(-?\)\(\)\([0-9]+\)$'], (Sign, Base, Value))
           else([
              error(Bad syntax for immediate field: "m5_Field".)
           ])
        ])
     ])
     /Convert to binary.
     var(Bin, m5_calc(m5_Base\m5_Value, 2, m5_NumBits))
     /If negative, convert to two's complement.
     if_eq(m5_Sign, -, [
        set(Bin, m5__asm_binary_negative(m5_Bin))
     ])
     ~Bin
  })

  
  /Zero-extend to n bits. E.g. m5_asm_zero_ext(1001, 7) => 0001001
  macro(asm_zero_ext, ['m5_substr_eval(00000000000000000000, 0, m5_calc($2 - m5_length($1)))$1'])
  /Extract bits from a binary immediate value.
  /m5_asm_imm_field(binary-imm, imm-length, max-bit, min-bit)
  /E.g. m5_asm_imm_field(101011, 17, 7, 3) => 5'b00101
  macro(asm_imm_field, ['m5_calc($3 - $4 + 1)'b['\m5_substr_eval(m5_asm_zero_ext($1, $2), ']m5_calc($2 - $3 - 1)[', ']m5_calc($3 - $4 + 1)[')']'])
  /Register operand.
  macro(asm_reg, ['5'd\m5_substr_eval(m5_abi_to_reg(['$1']), 1)'])

  /For debug, a string for an asm instruction.
  macro(asm_instr_str, [
     var(Str, ['($1) $2 ']m4_quote(m5_shift(m5_shift($@))))
     universal_var(['instr_str']m5_NUM_INSTRS, m5_Str\m5_substr_eval(['                                        '], m5_length(m5_Str)))
  ])
  /Assemble an instruction.
  /m5_asm(FOO, ...) defines m5_inst# as m5_comma_shiftFOO(...), counts instructions in m5_NUM_INSTRS ,and outputs a comment.
  fn(asm, mnemonic, ..., {
     universal_var(['instr']m5_NUM_INSTRS, m5_call(['asm_']m5_mnemonic\m5_comma_fn_args()))
     ~(['/']['/ Inst #']m5_NUM_INSTRS: m5_mnemonic m5_call(join, [', ']m5_comma_fn_args()))
     increment(NUM_INSTRS)
  })
  
  //=========
///M4-generated code. (Note: different indentation)
\m5
   TLV_fn(riscv_gen, [
      /The only output is for localparams, so squash all output if no localparams.
      ~(m5_if(m5_use_localparams, m5_quote(m5_riscv_gen_guts())))
   ])
   
   /Define the mapping of a single CSR.
   macro(_def_csr, ['m5_universal_var(csr_$1_arg, ['$2'])'])
   
   fn(riscv_gen_guts, [
      /(Output is squashed by caller unless m5_use_localparams.)
      /v---------------------
      /Instruction characterization

      /CSR Map. TODO: Merge this with CSR definitions.
      _def_csr(fflags,             1)
      _def_csr(frm,               10)
      _def_csr(fcsr,              11)
      _def_csr(cycle,   110000000000)
      _def_csr(time,    110000000001)
      _def_csr(instret, 110000000010)
      _def_csr(cycleh,  110010000000)
      _def_csr(timeh,   110010000001)
      _def_csr(instreth,110010000010)

      /For each opcode[6:2]
      /(User ISA Manual 2.2, Table 19.1)
      /Associate opcode[6:2] ([1:0] are 2'b11) with mnemonic and instruction type.
      /Instruction type is not in the table, but there seems to be a single instruction type for each of these,
      /so that is mapped here as well.
      /~op5(bits, type, mnemonic)   /// (produces localparam output for \SV_plus context)
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
      ~op5(01011, R, AMO)  /// (R-type, but rs2 = const for some, based on funct7 which doesn't exist for I-type?? R-type w/ ignored R2?)
      ~op5(01100, R, OP)
      ~op5(01101, U, LUI)
      ~op5(01110, R, OP_32)
      ~op5(01111, _, 64B)
      ~op5(10000, R4, MADD)
      ~op5(10001, R4, MSUB)
      ~op5(10010, R4, NMSUB)
      ~op5(10011, R4, NMADD)
      ~op5(10100, R, OP_FP)  /// (R-type, but rs2 = const for some, based on funct7 which doesn't exist for I-type?? R-type w/ ignored R2?)
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

      /For each instruction type, a mask, where each bit indicates whether
      /the op5 corresponding to the bit position is of the given type.
      /(m5_instr calls produce localparam definitions for \SV_plus context.)
      ~nl(\SV_plus)
      ~(m5_instr_types_sv(m5_instr_types_args()))

      ~nl(['// Instruction characterization.'])
      ~nl(['// (User ISA Manual 2.2, Table 19.2)'])
      ~nl(\SV_plus)
      /str(type,  /// (this is simply verified vs. op5)
      /    |  bit-width,
      /    |  |   extension, 
      /    |  |   |  opcode[6:2],  /// (aka op5)
      /    |  |   |  |      func3,   /// (if applicable)
      /    |  |   |  |      |    mnemonic)
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
      /~instr(_, 32, I, 00011, 000, FENCE)
      /~instr(_, 32, I, 00011, 001, FENCE_I)
      /~instr(_, 32, I, 11100, 000, ECALL_EBREAK)  /// Two instructions distinguished by an immediate bit, treated as a single instruction.
      ~instr(I, 32, I, 11100, 001, CSRRW)
      ~instr(I, 32, I, 11100, 010, CSRRS)
      ~instr(I, 32, I, 11100, 011, CSRRC)
      ~instr(I, 32, I, 11100, 101, CSRRWI)   /// TODO: I don't believe these immediate CSR instructions support assembly with an immediate specified in place of rs1.
      ~instr(I, 32, I, 11100, 110, CSRRSI)
      ~instr(I, 32, I, 11100, 111, CSRRCI)
      ~instr(I, 64, I, 00000, 110, LWU)
      ~instr(I, 64, I, 00000, 011, LD)
      ~instr(S, 64, I, 01000, 011, SD)
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
      ~instr(R4, 32, F, 10000, rm, 00, FMADD_S)
      ~instr(R4, 32, F, 10001, rm, 00, FMSUB_S)
      ~instr(R4, 32, F, 10010, rm, 00, FNMSUB_S)
      ~instr(R4, 32, F, 10011, rm, 00, FNMADD_S)
      ~instr(R, 32, F, 10100, rm, 0000000, FADD_S)
      ~instr(R, 32, F, 10100, rm, 0000100, FSUB_S)
      ~instr(R, 32, F, 10100, rm, 0001000, FMUL_S)
      ~instr(R, 32, F, 10100, rm, 0001100, FDIV_S)
      ~instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRT_S)
      ~instr(R, 32, F, 10100, 000, 0010000, FSGNJ_S)
      ~instr(R, 32, F, 10100, 001, 0010000, FSGNJN_S)
      ~instr(R, 32, F, 10100, 010, 0010000, FSGNJX_S)
      ~instr(R, 32, F, 10100, 000, 0010100, FMIN_S)
      ~instr(R, 32, F, 10100, 001, 0010100, FMAX_S)
      ~instr(R2, 32, F, 10100, rm, 1100000, 00000, FCVT_W_S)
      ~instr(R2, 32, F, 10100, rm, 1100000, 00001, FCVT_WU_S)
      ~instr(R2, 32, F, 10100, 000, 1110000, 00000, FMV_X_W)
      ~instr(R, 32, F, 10100, 010, 1010000, FEQ_S)
      ~instr(R, 32, F, 10100, 001, 1010000, FLT_S)
      ~instr(R, 32, F, 10100, 000, 1010000, FLE_S)
      ~instr(R2, 32, F, 10100, 001, 1110000, 00000, FCLASS_S)
      ~instr(R2, 32, F, 10100, rm, 1101000, 00000, FCVT_S_W)
      ~instr(R2, 32, F, 10100, rm, 1101000, 00001, FCVT_S_WU)
      ~instr(R2, 32, F, 10100, 000, 1111000, 00000, FMV_W_X)
      ~instr(R2, 64, F, 10100, rm, 1100000, 00010, FCVT_L_S)
      ~instr(R2, 64, F, 10100, rm, 1100000, 00011, FCVT_LU_S)
      ~instr(R2, 64, F, 10100, rm, 1101000, 00010, FCVT_S_L)
      ~instr(R2, 64, F, 10100, rm, 1101000, 00011, FCVT_S_LU)
      ~instr(I, 32, D, 00001, 011, FLD)
      ~instr(S, 32, D, 01001, 011, FSD)
      ~instr(R4, 32, D, 10000, rm, 01, FMADD_D)
      ~instr(R4, 32, D, 10001, rm, 01, FMSUB_D)
      ~instr(R4, 32, D, 10010, rm, 01, FNMSUB_D)
      ~instr(R4, 32, D, 10011, rm, 01, FNMADD_D)
      ~instr(R, 32, D, 10100, rm, 0000001, FADD_D)
      ~instr(R, 32, D, 10100, rm, 0000101, FSUB_D)
      ~instr(R, 32, D, 10100, rm, 0001001, FMUL_D)
      ~instr(R, 32, D, 10100, rm, 0001101, FDIV_D)
      ~instr(R2, 32, D, 10100, rm, 0101101, 00000, FSQRT_D)
      ~instr(R, 32, D, 10100, 000, 0010001, FSGNJ_D)
      ~instr(R, 32, D, 10100, 001, 0010001, FSGNJN_D)
      ~instr(R, 32, D, 10100, 010, 0010001, FSGNJX_D)
      ~instr(R, 32, D, 10100, 000, 0010101, FMIN_D)
      ~instr(R, 32, D, 10100, 001, 0010101, FMAX_D)
      ~instr(R2, 32, D, 10100, rm, 0100000, 00001, FCVT_S_D)
      ~instr(R2, 32, D, 10100, rm, 0100001, 00000, FCVT_D_S)
      ~instr(R, 32, D, 10100, 010, 1010001, FEQ_D)
      ~instr(R, 32, D, 10100, 001, 1010001, FLT_D)
      ~instr(R, 32, D, 10100, 000, 1010001, FLE_D)
      ~instr(R2, 32, D, 10100, 001, 1110001, 00000, FCLASS_D)
      ~instr(R2, 32, D, 10100, rm, 1110001, 00000, FCVT_W_D)
      ~instr(R2, 32, D, 10100, rm, 1100001, 00001, FCVT_WU_D)
      ~instr(R2, 32, D, 10100, rm, 1101001, 00000, FCVT_D_W)
      ~instr(R2, 32, D, 10100, rm, 1101001, 00001, FCVT_D_WU)
      ~instr(R2, 64, D, 10100, rm, 1100001, 00010, FCVT_L_D)
      ~instr(R2, 64, D, 10100, rm, 1100001, 00011, FCVT_LU_D)
      ~instr(R2, 64, D, 10100, 000, 1110001, 00000, FMV_X_D)
      ~instr(R2, 64, D, 10100, rm, 1101001, 00010, FCVT_D_L)
      ~instr(R2, 64, D, 10100, rm, 1101001, 00011, FCVT_D_LU)
      ~instr(R2, 64, D, 10100, 000, 1111001, 00000, FMV_D_X)
      ~instr(I, 32, Q, 00001, 100, FLQ)
      ~instr(S, 32, Q, 01001, 100, FSQ)
      ~instr(R4, 32, Q, 10000, rm, 11, FMADD_Q)
      ~instr(R4, 32, Q, 10001, rm, 11, FMSUB_Q)
      ~instr(R4, 32, Q, 10010, rm, 11, FNMSUB_Q)
      ~instr(R4, 32, Q, 10011, rm, 11, FNMADD_Q)
      ~instr(R, 32, Q, 10100, rm, 0000011, FADD_Q)
      ~instr(R, 32, Q, 10100, rm, 0000111, FSUB_Q)
      ~instr(R, 32, Q, 10100, rm, 0001011, FMUL_Q)
      ~instr(R, 32, Q, 10100, rm, 0001111, FDIV_Q)
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
      /~instr(R4, 32, B, 01100, 001, 11, CMIX)  /// Currently we are excluing ternary BMI's for optimization, although its only a draft
      /~instr(R4, 32, B, 01100, 101, 11, CMOV)
      /~instr(R4, 32, B, 01100, 001, 10, FSL)
      /~instr(R4, 32, B, 01100, 101, 10, FSR)
      /~instr(R4, 32, B, 00100, 101, 10, FSRI)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00000, CLZ) /// single operand instruction are currently using R2-type rather than I-type encoding
      ~instr(R2, 32, B, 01100, 001, 0110000, 00001, CTZ)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00010, PCNT)
      /~instr(R2, 64, B, 01100, 001, 0110000, 00011, BMATFLIP)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00100, SEXT_B)
      ~instr(R2, 32, B, 01100, 001, 0110000, 00101, SEXT_H)
      ~instr(R2, 32, B, 01100, 001, 0110000, 10000, CRC32_B)
      ~instr(R2, 32, B, 01100, 001, 0110000, 10001, CRC32_H)
      ~instr(R2, 32, B, 01100, 001, 0110000, 10010, CRC32_W)
      /~instr(R2, 64, B, 01100, 001, 0110000, 10011, CRC32_D)
      ~instr(R2, 32, B, 01100, 001, 0110000, 11000, CRC32C_B)
      ~instr(R2, 32, B, 01100, 001, 0110000, 11001, CRC32C_H)
      ~instr(R2, 32, B, 01100, 001, 0110000, 11010, CRC32C_W)
      /~instr(R2, 64, B, 01100, 001, 0110000, 11011, CRC32C_D)
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
      /~instr(R, 64, B, 01100, 011, 0000100, BMATOR)
      /~instr(R, 64, B, 01100, 011, 0100100, BMATXOR)
      ~instr(R, 32, B, 01100, 111, 0000100, PACKH)
      ~instr(R, 32, B, 01100, 111, 0100100, BFP)
      ~instr(If, 32, B, 00100, 001, 000010, SHFLI)
      ~instr(If, 32, B, 00100, 101, 000010, UNSHFLI)
      /~instr(I, 64, B, 01100, 100, ADDIWU)
      /~instr(If, 64, B, 01100, 001, 000010, SLLI_UW)
      /~instr(R, 64, B, 01110, 000, 0000101, ADDWU)
      /~instr(R, 64, B, 01110, 000, 0100101, SUBWU)
      /~instr(R, 64, B, 01110, 000, 0000100, ADD_UW)
      /~instr(R, 64, B, 01110, 000, 0100100, SUBU_W)
      /~instr(R, 64, B, 01110, 001, 0010000, SLOW)
      /~instr(R, 64, B, 01110, 101, 0010000, SROW)
      /~instr(R, 64, B, 01110, 001, 0110000, ROLW)
      /~instr(R, 64, B, 01110, 101, 0110000, RORW)
      /~instr(R, 64, B, 01110, 001, 0100100, SBCLRW)
      /~instr(R, 64, B, 01110, 001, 0010100, SBSETW)
      /~instr(R, 64, B, 01110, 001, 0110100, SBINVW)
      /~instr(R, 64, B, 01110, 101, 0100100, SBEXTW)
      /~instr(R, 64, B, 01110, 101, 0010100, GROCW)
      /~instr(R, 64, B, 01110, 101, 0110100, GREVW)
      /~instr(If, 64, B, 00110, 001, 001000, SLOIW)
      /~instr(If, 64, B, 00110, 101, 001000, SROIW)
      /~instr(If, 64, B, 00110, 101, 011000, RORIW)
      /~instr(If, 64, B, 00110, 001, 010010, SBCLRIW)
      /~instr(If, 64, B, 00110, 001, 001010, SBSETIW)
      /~instr(If, 64, B, 00110, 001, 011010, SBINVIW)
      /~instr(If, 64, B, 00110, 101, 001010, GORCIW)
      /~instr(If, 64, B, 00110, 101, 011010, GREVIW)
      /~instr(R4, 64, B, 01110, 001, 10, FSLW)
      /~instr(R4, 64, B, 01110, 101, 10, FSRW)
      /~instr(R4, 64, B, 00110, 101, 10, FSRIW) //rs2 is "imm"
      /~instr(R2, 64, B, 00110, 001, 0110000, 00000, CLZW)
      /~instr(R2, 64, B, 00110, 001, 0110000, 00001, CTZW)
      /~instr(R2, 64, B, 00110, 001, 0110000, 00010, PCNTW)
      /~instr(R, 64, B, 01110, 001, 0000101, CLMULW)
      /~instr(R, 64, B, 01110, 010, 0000101, CLMULRW)
      /~instr(R, 64, B, 01110, 011, 0000101, CLMULHW)
      /~instr(R, 64, B, 01110, 001, 0000100, SHFLW)
      /~instr(R, 64, B, 01110, 101, 0000100, UNSHFLW)
      /~instr(R, 64, B, 01110, 110, 0100100, BDEPW)
      /~instr(R, 64, B, 01110, 110, 0000100, BEXTW)
      /~instr(R, 64, B, 01110, 100, 0000100, PACKW)
      /~instr(R, 64, B, 01110, 100, 0100100, PACKUW)
      /~instr(R, 64, B, 01110, 111, 0100100, BFPW)
      
      /Pseudoinstructions that are replaced by a single instruction.
      _pseudoinstr(MV, 2, ADDI, #1, #2, 0)
      _pseudoinstr(NOT, 2, XORI, #1, #2, 111111111111)
      _pseudoinstr(NEG, 2, SUB, #1, zero, #2)
      _pseudoinstr(NEGW, 2, SUBW, #1, zero, #2)
      _pseudoinstr(SEXT_W, 2, ADDIW, #1, #2, 0)
      _pseudoinstr(ZEXT_B, 2, ANDI, #1, #2, 11111111)
      _pseudoinstr(SEQZ, 2, SLTIU, #1, #2, 1)
      _pseudoinstr(SNEZ, 2, SLTU, #1, zero, #2)
      _pseudoinstr(SLTZ, 2, SLT, #1, #2, zero)
      _pseudoinstr(SGTZ, 2, SLT, #1, zero, #2)
      _pseudoinstr(FMV_S, 2, FSGNJ_S, #1, #2, #2)
      _pseudoinstr(FABS_S, 2, FSGNJX_S, #1, #2, #2)
      _pseudoinstr(FNEG_S, 2, FSGNJN_S, #1, #2, #2)
      _pseudoinstr(FMV_D, 2, FSGNJ_D, #1, #2, #2)
      _pseudoinstr(FABS_D, 2, FSGNJX_D, #1, #2, #2)
      _pseudoinstr(FNEG_D, 2, FSGNJN_D, #1, #2, #2)
      _pseudoinstr(BEQZ, 2, BEQ, #1, zero, #2)
      _pseudoinstr(BNEZ, 2, BNE, #1, zero, #2)
      _pseudoinstr(BLEZ, 2, BGE, zero, #1, #2)
      _pseudoinstr(BGEZ, 2, BGE, #1, zero, #2)
      _pseudoinstr(BLTZ, 2, BLT, #1, zero, #2)
      _pseudoinstr(BGTZ, 2, BLT, zero, #1, #2)
      _pseudoinstr(BGT, 3, BLT, #2, #1, #3)
      _pseudoinstr(BLE, 3, BGE, #2, #1, #3)
      _pseudoinstr(BGTU, 3, BLTU, #2, #1, #3)
      _pseudoinstr(BLEU, 3, BGEU, #2, #1, #3)
      _pseudoinstr(J, 1, JAL, zero, #1)
      _pseudoinstr(JR, 1, JALR, zero, 0(#1))
      _pseudoinstr(RET, 0, JALR, zero, 0(ra))
      _pseudoinstr(RDINSTRET, 1, CSRRS, #1, instret, zero)   /// TODO: Fix instret and other CSR use below.
      _pseudoinstr(RDINSTRETH, 1, CSRRS, #1, instreth, zero)
      _pseudoinstr(RDCYCLE, 1, CSRRS, #1, cycle, zero)
      _pseudoinstr(RDCYCLEH, 1, CSRRS, #1, cycleh, zero)
      _pseudoinstr(RDTIME, 1, CSRRS, #1, time, zero)
      _pseudoinstr(RDTIMEH, 1, CSRRS, #1, timeh, zero)
      _pseudoinstr(CSRR, 2, CSRRS, #1, #2, zero)
      _pseudoinstr(CSRW, 2, CSRRW, zero, #1, #2)
      _pseudoinstr(CSRS, 2, CSRRS, zero, #1, #2)
      _pseudoinstr(CSRC, 2, CSRRC, zero, #1, #2)
      _pseudoinstr(CSRWI, 2, CSRRWI, zero, #1, #2)
      _pseudoinstr(CSRSI, 2, CSRRSI, zero, #1, #2)
      _pseudoinstr(CSRCI, 2, CSRRCI, zero, #1, #2)
      _pseudoinstr(FRCSR, 1, CSRRS, #1, fcsr, zero)
      _pseudoinstr(FSCSR, 2, CSRRW, #1, fcsr, #2)
      /_pseudoinstr(FSCSR, 1, CSRRW, zero, fcsr, #1)  /// TODO: 1-operand version of the line above. Support this.
      _pseudoinstr(FRRM, 1, CSRRS, #1, frm, zero)
      _pseudoinstr(FSRM, 2, CSRRW, #1, frm, #2)
      /_pseudoinstr(FSRM, 1, CSRRW, zero, frm, #1)
      _pseudoinstr(FSRMI, 2, CSRRWI, #1, frm, #2)
      /_pseudoinstr(FSRMI, 1, CSRRWI, zero, frm, #1)
      _pseudoinstr(FRFLAGS, 1, CSRRS, #1, fflags, zero)
      _pseudoinstr(FSFLAGS, 2, CSRRW, #1, fflags, #2)
      /_pseudoinstr(FSFLAGS, 1, CSRRW, zero, fflags, #1)
      _pseudoinstr(FSFLAGSI, 2, CSRRWI, #1, fflags, #2)
      /_pseudoinstr(FSFLAGSI, 1, CSRRWI, zero, fflags, #1)
      /TODO: Some didn't fit the mold:
      /Many translate to multiple instructions.
      /Some accept a subset of args.
      /  jal offset	=> jal ra, offset
      /  jalr rs =>	jalr ra, rs, 0
      /  fence =>	fence iorw, iorw
      /**
      _exp_pseudoinstr(LA)
      _exp_pseudoinstr(LLA)
      _exp_pseudoinstr(LGA)
      _exp_pseudoinstr(LB)
      _exp_pseudoinstr(LH)
      _exp_pseudoinstr(LW)
      _exp_pseudoinstr(LD)
      _exp_pseudoinstr(SB)
      _exp_pseudoinstr(SH)
      _exp_pseudoinstr(SW)
      _exp_pseudoinstr(SD)
      _exp_pseudoinstr(FLW)
      _exp_pseudoinstr(FLD)
      _exp_pseudoinstr(FSW)
      _exp_pseudoinstr(FSD)
      _exp_pseudoinstr(LI)
      _exp_pseudoinstr(SEXT_B)
      _exp_pseudoinstr(SEXT_H)
      _exp_pseudoinstr(ZEXT_H)
      _exp_pseudoinstr(ZEXT_W)
      _exp_pseudoinstr(CALL)
      _exp_pseudoinstr(TAIL)
      _exp_pseudoinstr()
      _exp_pseudoinstr()
      _exp_pseudoinstr()
      _exp_pseudoinstr()
      _exp_pseudoinstr()
      **/
   ])
   
   
   /Create macro _pseudoinstr(<mnemonic>, <num-pseudoinstr-args>, <actual-instr>, <actual-arg-1>, <actual-arg-2>, ...)
   /Actual arg may contain, e.g. #2 to represent the second argument of the pseudoinstruction.
   /E.g. _pseudoinstr(MV, 2, ADDI, #1, #2, 0)
   /TODO: This would be cleaner using fn's at both levels and ^ in the inner fn.
   macro(_pseudoinstr, [
      if_var_def(instr_defined_$3, [   /// This conditions the pseudo-instruction on support for the same extension as the instruction it maps to.
         macro(['_pseudoinstr_$1'], [
            verify_num_args(['$1'], ['$2'], ']m5_quote(['$']['#'])[')
            ~call(assemble_instr, ['$3'], ']']m5_nquote(2, m5_call(_pseudoinstr_args, m5_comma_shift(m5_shift(m5_shift($@)))))['[')
         ])
      ])
   ])
   /For m5_pseudoinstr, turn the actual-instruction argument list into <args> for m5_asm(<instr>, <args>).
   /E.g.: m5_pseudoinstr_args([''], #1, #2, 0) => ['$1'],['$2'],['0']
   fn(_pseudoinstr_args,
      comma: ['separator; [''], and [','] thereafter'],
      ...: ['actual-instruction args from _pseudoinstr'],
   {
      ~comma
      var_regex(['$1'], ['^\(.*\)#\([0-9]\)\(.*\)'], (pre, num, post))
      ~if_so([
         /"#" arg
         ~(m5_pre['$']m5_num\m5_post)
      ])
      ~else([
         /Use the arg literally.
         ~(['$1'])
      ])
      ~if($# > 1, [
         ~_pseudoinstr_args([','], m5_shift($@))
      ])
   })


   /=========
   /Assembler
   /=========
   
   /These functions go from proper RISC-V assembly code to the pseudo-assembly defined by m5_asm functions.
   doc_fn(abi_to_reg, ['
      D: Map an ABI register name. ABI name -> register name. If field_type is given,
      the type is checked and only the index is returned; o.w. the type is returned
      as the first character, e.g. "x3". For unknown registers, type is "-" and
      a register index is returned that is in range, though meaningless.
   '],
      abi: ['ABI or register name'],
      ?field_type: ['[x|f] register type'],
   {
      /Parse ABI reg name.
      var_regex(m5_abi, ['\([a-z]+\)\([0-9]*\)'], (name, num))
      
      /Look up mapping.
      if_var_def(_REG_OF_ABI_NAME_\m5_uppercase(m5_abi), [
         var_regex(m5_get(['_REG_OF_ABI_NAME_']m5_uppercase(m5_abi)), ['\([a-z]+\)\([0-9]*\)'], (reg_type2, reg_index2))
         else([
            error(['BUG: Failed to pattern match ABI type and index.'])
            set(reg_type2, -, reg_index, 0)
         ])
      ])
      else_if(m5_eq(m5_name, x) || m5_eq(m5_name, f), [
         /x# or f# register.
         var(reg_type2, m5_name, reg_index2, m5_num)
         if(m5_num > 31, [
            error(['Register ']m5_abi[' index out of range.'])
            set(reg_index2, 0)
         ])
      ])
      else([
         error(['Unrecognized ABI register name: "']m5_abi['".'])
         var(reg_type2, -, reg_index2, 0)
      ])
      
      /Return type and index.
      ~if_null(field_type, [
         ~reg_type2
      ], [
         if(m5_neq(m5_field_type, m5_reg_type2) && m5_neq(m5_field_type, -), [
            error(['Register field of type ']m5_field_type[' given ABI register ']m5_abi[' of type ']m5_reg_type2['.'])
         ])
      ])
      ~reg_index2
   })
   
   
   /-------------
   /ABI Registers
   /-------------
   
   /Define ABI to type/index and type/index to ABI mapping vars for the given ABI name, index, and type (x/f).
   /Some register indices have more than one ABI name. Both will be pushed, so the last call
   /determines the index-to-ABI mapping.
   fn(_map_abi_name, abi_name, type, index, [
      /DEBUG(['Defining: "']m5_abi_name['" = ']m5_type\m5_index['.'])
      universal_var(_REG_OF_ABI_NAME_\m5_uppercase(m5_abi_name), m5_type\m5_index)
      if_var_def(_ABI_NAME_OF_\m5_uppercase(m5_type)\m5_index, [''], [
         universal_var(_ABI_NAME_OF_\m5_uppercase(m5_type)\m5_index, m5_lowercase(m5_abi_name))
      ])
   ])
   /Define X/F registers, given a list of ABI names.
   fn(_map_abi_names, type, ..., [
      var(index, m5_calc(32 - $#))
      _map_abi_name(['$1'], m5_type, m5_index)    /// Map, e.g. "gp" -> x3.
      if($# > 1, [
         /Recurse.
         _map_abi_names(m5_type, m5_shift($@))
      ])
   ])
   
   /Define ABI registers.
   /
   /Define alternate ABI names.
   _map_abi_name(fp, x, 8)
   /Map the X registers.
   _map_abi_names(x, zero, ra, sp, gp, tp, t0, t1, t2, s0, s1, a0, a1, a2, a3, a4, a5, a6, a7, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, t3, t4, t5, t6)
   /Map the F registers.
   _map_abi_names(f, ft0, ft1, ft2, ft3, ft4, ft5, ft6, ft7, fs0, fs1, fa0, fa1, fa2, fa3, fa4, fa5, fa6, fa7, fs2, fs3, fs4, fs5, fs6, fs7, fs8, fs9, fs10, fs11, ft8, ft9, ft10, ft11)


   /Construct a JavaScript arrays for the register maps.
   universal_var(js_abi_x_map, [)
   universal_var(js_abi_f_map, [)
   eval({
      fn(js_abi, cnt, [
         append_var(js_abi_x_map, ['"']m5_eval(m5_get(['_ABI_NAME_OF_X']m5_cnt))['", '])
         append_var(js_abi_f_map, ['"']m5_eval(m5_get(['_ABI_NAME_OF_F']m5_cnt))['", '])
         if(m5_cnt < 31, [
            js_abi(m5_calc(m5_cnt + 1))
         ])
      ])
      js_abi(0)
      append_var(js_abi_x_map, ])
      append_var(js_abi_f_map, ])
   })
   
   
   /--------
   /Assemble
   /--------
   
   /Assemble a real instruction (not a pseudoinstruction).
   fn(assemble_instr, mnemonic, fields, {
      macro(bad, ['m5_error(['Malformed instruction: ']m5_mnemonic[''](m5_fields))'])
      
      /DEBUG(['Found instruction=: ']m5_mnemonic[''](m5_fields))
      /Parse format based on instruction characteristics.
      
      var(op5, m5_get(['op5_of_instr_']m5_mnemonic))
      ~if(m5_eq(m5_op5, m5_op5_named_LOAD) ||
         m5_eq(m5_op5, m5_op5_named_LOAD_FP) ||
         m5_eq(m5_op5, m5_op5_named_STORE) ||
         m5_eq(m5_op5, m5_op5_named_STORE_FP) ||
         m5_eq(m5_mnemonic, JALR),
      [
         /Format should be, e.g. LB a0, 0(a2)
         var_regex(m5_fields, ['^\(\w+\),\s*\(-?\w+\)(\(\w+\))$'], (r1, imm, r2))
         ~if_so([
            ~asm(m5_mnemonic, m5_r1, m5_r2, m5_imm)
         ])
         else(['m5_bad()'])
      ])
      ~else_if_regex(m5_mnemonic, ['^\(CSR\)'], (dummy), [
         /DEBUG(['CSR instruction: ']m5_mnemonic[''](m5_fields))
         /CSR instruction.
         /CSR instructions take a CSR as the immediate, and for some reason, the operands are in a different order.
         /Decode the CSR.
         fn(process_fields, rd, csr, rs, {
            var(csr_arg, m5_if_def(['csr_']m5_csr['_arg'], ['m5_csr_']m5_csr['_arg'], ['m5_csr']))
            ~asm(m5_mnemonic, m5_rd, m5_rs, m5_csr_arg)
         })
         ~process_fields(m5_eval(m5_fields))
      ])
      ~else([
         /DEBUG(['Typical instruction: ']m5_mnemonic[''](m5_fields))
         /Format, any number of comma-separated fields: e.g. ADDI t0, t2, 1
         if_neq(m5_fields, [''], [
            set(fields, [', ']m5_fields)
         ])
         var_regex(m5_fields, ['^\(,\s*\w+\)*$'], (dummy))
         ~if_so([
            ~asm(m5_mnemonic\m5_eval(m5_fields))
         ])
         else(['m5_bad()'])
      ])
      ~nl
   })
   
   fn(assemble_line, line_cnt, line, {
      /DEBUG(Assembling line m5_line_cnt: m5_line)
      /Strip comment and trailing whitespace from m5_line.
      var(pos, m5_index_of(m5_line, ['#']))
      if(m5_pos >= 0, [
         set(line, m5_substr(m5_line, 0, m5_pos))
      ])
      strip_trailing_whitespace_from(line)
      
      /Parse (uncommented) line, producing IMem value definition and commented SV line.
      ~if_regex(m5_line, ['^\(\s*\)$'], (dummy), {
         
         /
         /Empty line
         /
         
         /DEBUG(['Found empty line.'])
      
      }, ['^\s+\(\w+\)\s*\(.*\)'], (mnemonic, fields), {
         
         /
         /Instruction
         /
         
         /Convert given mnemonic to internal mnemonic, e.g. sext.b -> SEXT_B
         set(mnemonic, m5_translit(m5_uppercase(m5_mnemonic), ['.'], ['_']))
      
         if_def(['_expanding_pseudoinstr_']m5_mnemonic, [
            /Pseudoinstruction that expands to multiple instructions.
            error(['This pseudoinstruction is not yet supported.'])
         ])
         ~else_if_def(['_pseudoinstr_']m5_mnemonic, [
            /Simple pseudoinstructions.
            ~call(['_pseudoinstr_']m5_mnemonic\m5_if_eq(m5_fields, [''], [''], [',m5_eval(m5_fields)']))
         ])
         ~else([
            /Instruction (not a pseudoinstruction)
            ~assemble_instr(m5_mnemonic, m5_fields)
         ])
         
      }, ['^\.\(\w+\)\(\)?'], (directive, fields), {
      
         /
         /Directive
         /
         
         /DEBUG(['Found directive: ']m5_directive m5_fields)
      
      }, ['^\(\w+\):\s*$'], (label), {
         
         /
         /Label
         /
         
         define_label(m5_label, m5_NUM_INSTRS)
      
      }, {
         error(['Could not parse assembly code line: "']m5_line['"'])
      })
   })
   
   /Assemble a block of assembly code.
   /The first line defines the indentation of the code, and indentation is removed before processing.
   fn(assemble, code, {
      /DEBUG(Assembling code: m5_code)
      var(assemble__cnt, 0)
      ~for_each_line(m5_code, [
         ~assemble_line(m5_assemble__cnt, m5_Line)
         increment(assemble__cnt)
      ])
   })
