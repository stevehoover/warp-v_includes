\m4_TLV_version 1d: tl-x.org
\SV

m4+definitions(['
  // --------------------------------------
  // Associate each op5 value with an instruction type.
  // --------------------------------------

  // TODO:
  // We construct M4_OP5_XXXXX_TYPE, and verify each instruction against that.
  // Instruction fields are constructed and valid based on op5.
  //...
  // TO UPDATE:
  // We construct localparam INSTR_TYPE_X_MASK as a mask, one bit per op5 indicating whether the op5 is of the type.
  // Instantiated recursively for each instruction type.
  // Initializes m4_instr_type_X_mask_expr which will build up a mask, one bit per op5.
  m4_define(['m4_instr_types'],
            ['m4_ifelse(['$1'], [''], [''],
                        ['m4_define(['m4_instr_type_$1_mask_expr'], ['0'])m4_instr_types(m4_shift($@))'])'])
  // Instantiated recursively for each instruction type in \SV_plus context after characterizing each type.
  // Declares localparam INSTR_TYPE_X_MASK as m4_instr_type_X_mask_expr.
  m4_define(['m4_instr_types_sv'],
            ['m4_ifelse(['$1'], [''], [''],
                        ['localparam INSTR_TYPE_$1_MASK = m4_instr_type_$1_mask_expr; m4_instr_types_sv(m4_shift($@))'])'])
  // Instantiated recursively for each instruction type in \SV_plus context to decode instruction type.
  // Creates "assign $$is_x_type = INSTR_TYPE_X_MASK[$raw_op5];" for each type.
  m4_define(['m4_types_decode'],
            ['m4_ifelse(['$1'], [''], [''],
                        ['['assign $$is_']m4_translit(['$1'], ['A-Z'], ['a-z'])['_type = INSTR_TYPE_$1_MASK[$raw_op5]; ']m4_types_decode(m4_shift($@))'])'])
  // Instantiated for each op5 in \SV_plus context.
  m4_define(['m4_op5'],
            ['m4_define(['M4_OP5_$1_TYPE'], $2)['localparam [4:0] OP5_$3 = 5'b$1;']m4_define(['m4_instr_type_$2_mask_expr'], m4_quote(m4_instr_type_$2_mask_expr)[' | (1 << 5'b$1)'])'])


  // --------------------------------
  // Characterize each instruction mnemonic
  // --------------------------------

  // Each instruction is defined by instantiating m4_instr(...), e.g.: 
  //    m4_instr(B, 32, I, 11000, 000, BEQ)
  // which instantiates an instruction-type-specific macro, e.g.:
  //    m4_instrB(32, I, 11000, 000, BEQ)
  // which produces (or defines macros for):
  //   o instruction decode logic ($is_<mnemonic>_instr = ...;)
  //   o for debug, an expression to produce the MNEMONIC.
  //   o result MUX expression to select result of the appropriate execution expression
  //   o $illegal_instruction expression
  //   o localparam definitions for fields
  //   o m4_asm(<MNEMONIC>, ...) to assemble instructions to their binary representations

  // Return 1 if the given instruction is supported, [''] otherwise.
  // m4_instr_supported(<args-of-m4_instr(...)>)
  m4_define(['m4_instr_supported'],
            ['m4_ifelse(M4_EXT_$3, 1,
                        ['m4_ifelse(M4_WORD_CNT, ['$2'], 1, [''])'],
                        [''])'])

  // Instantiated (in \SV_plus context) for each instruction.
  // m4_instr(<instr-type-char(s)>, <type-specific-args>)
  // This instantiates m4_instr<type>(type-specific-args>)
  m4_define_hide(['m4_instr'],
                 ['// check instr type

                   m4_ifelse(M4_OP5_$4_TYPE, m4_ifdef(['m4_instr_type_of_$1'], ['m4_instr_type_of_$1'], ['$1']), [''],
                             ['m4_errprint(['Instruction ']m4_argn($#, $@)[''s type ($1) is inconsistant with its op5 code ($4) of type ']M4_OP5_$4_TYPE[' on line ']m4___line__[' of file ']m4_FILE.m4_new_line)'])
                   // if instrs extension is supported and instr is for the right machine width, "
                   m4_ifelse(m4_instr_supported($@), 1, ['m4_show(['localparam [6:0] ']']m4_argn($#, $@)['['_INSTR_OPCODE = 7'b$4['']11;m4_instr$1(m4_shift($@))'])'],
                             [''])'])


  // Decode logic for instructions with various opcode/func bits that dictate the mnemonic.
  // (This would be easier if we could use 'x', but Yosys doesn't support ==?/!=? operators.)
  // Helpers to deal with "rm" cases:
  m4_define(['m4_op5_and_funct3'],
            ['$raw_op5 == 5'b$3 m4_ifelse($4, ['rm'], [''], ['&& $raw_funct3 == 3'b$4'])'])
  m4_define(['m4_funct3_localparam'],
            ['m4_ifelse(['$2'], ['rm'], [''], [' localparam [2:0] $1_INSTR_FUNCT3 = 3'b$2;'])'])
  // m4_asm_<MNEMONIC> output for funct3 or rm, returned in unquoted context so arg references can be produced. 'rm' is always the last m4_asm_<MNEMONIC> arg (m4_arg(#)).
  //   Args: $1: MNEMONIC, $2: funct3 field of instruction definition (or 'rm')
  m4_define(['m4_asm_funct3'], ['['m4_ifelse($2, ['rm'], ['3'b']m4_argn(']m4_arg(#)[', m4_echo(']m4_arg(@)[')), ['$1_INSTR_FUNCT3'])']'])
  // Opcode + funct3 + funct7 (R-type, R2-type). $@ as for m4_instrX(..), $7: MNEMONIC, $8: number of bits of leading bits of funct7 to interpret. If 5, for example, use the term funct5, $9: (opt) for R2, the r2 value.
  m4_define(['m4_instr_funct7'],
            ['m4_instr_decode_expr($7, m4_op5_and_funct3($@)[' && $raw_funct7'][6:m4_eval(7-$8)][' == $8'b$5']m4_ifelse($9, [''], [''], [' && $raw_rs2 == 5'b$9']))m4_funct3_localparam(['$7'], ['$4'])[' localparam [$8-1:0] $7_INSTR_FUNCT$8 = $8'b$5;']'])
  // For cases w/ extra shamt bit that cuts into funct7.
  m4_define(['m4_instr_funct6'],
            ['m4_instr_decode_expr($7, m4_op5_and_funct3($@)[' && $raw_funct7[6:1] == 6'b$5'])m4_funct3_localparam(['$7'], ['$4'])[' localparam [6:0] $7_INSTR_FUNCT6 = 6'b$5;']'])
  // Opcode + funct3 + func7[1:0] (R4-type)
  m4_define(['m4_instr_funct2'],
            ['m4_instr_decode_expr($6, m4_op5_and_funct3($@)[' && $raw_funct7[1:0] == 2'b$5'])m4_funct3_localparam(['$6'], ['$4'])[' localparam [1:0] $6_INSTR_FUNCT2 = 2'b$5;']'])
  // Opcode + funct3 + funct7[6:2] (R-type where funct7 has two lower bits that do not distinguish mnemonic.)
  m4_define(['m4_instr_funct5'],
            ['m4_instr_decode_expr($6, m4_op5_and_funct3($@)[' && $raw_funct7[6:2] == 5'b$5'])m4_funct3_localparam(['$6'], ['$4'])[' localparam [4:0] $6_INSTR_FUNCT5 = 5'b$5;']'])
  // Opcode + funct3
  m4_define(['m4_instr_funct3'],
            ['m4_instr_decode_expr($5, m4_op5_and_funct3($@), $6)m4_funct3_localparam(['$5'], ['$4'])'])
  // Opcode
  m4_define(['m4_instr_no_func'],
            ['m4_instr_decode_expr($4, ['$raw_op5 == 5'b$3'])'])

  // m4_instr_decode_expr macro
  // Args: (MNEMONIC, decode_expr, (opt)['no_dest']/other)
  // Extends the following definitions to reflect the given instruction <mnemonic>:
  m4_define(['m4_decode_expr'], [''])          // instructiton decode: $is_<mnemonic>_instr = ...; ...
  m4_define(['m4_rslt_mux_expr'], [''])        // result combining expr.: ({32{$is_<mnemonic>_instr}} & $<mnemonic>_rslt) | ...
  m4_define(['m4_illegal_instr_expr'], [''])   // $illegal instruction exception expr: && ! $is_<mnemonic>_instr ...
  m4_define(['m4_mnemonic_expr'], [''])        // $is_<mnemonic>_instr ? "<MNEMONIC>" : ...
  m4_define_hide(
     ['m4_instr_decode_expr'],
     ['m4_define(
          ['m4_decode_expr'],
          m4_dquote(m4_decode_expr['$is_']m4_translit($1, ['A-Z'], ['a-z'])['_instr = $2;m4_plus_new_line   ']))
       m4_ifelse(['$3'], ['no_dest'],
          [''],
          ['m4_define(
             ['m4_rslt_mux_expr'],
             m4_dquote(m4_rslt_mux_expr[' |']['m4_plus_new_line       ({']M4_WORD_CNT['{$is_']m4_translit($1, ['A-Z'], ['a-z'])['_instr}} & $']m4_translit($1, ['A-Z'], ['a-z'])['_rslt)']))'])
       m4_define(
          ['m4_illegal_instr_expr'],
          m4_dquote(m4_illegal_instr_expr[' && ! $is_']m4_translit($1, ['A-Z'], ['a-z'])['_instr']))
       m4_define(
          ['m4_mnemonic_expr'],
          m4_dquote(m4_mnemonic_expr['$is_']m4_translit($1, ['A-Z'], ['a-z'])['_instr ? "$1']m4_substr(['          '], m4_len(['$1']))['" : ']))'])

  // The first arg of m4_instr(..) is a type, and a type-specific macro is invoked. Types are those defined by RISC-V, plus:
  //   R2: R-type with a hard-coded rs2 value. (assuming illegal instruction exception should be thrown for wrong value--not clear in RISC-V spec)
  //   If: I-type with leading bits of imm[11:...] used as function bits.

  m4_define(['m4_instr_type_of_R2'], ['R'])
  m4_define(['m4_instr_type_of_If'], ['I'])
  // Unique to each instruction type, eg:
  //   m4_instr(U, 32, I, 01101,      LUI)
  //   m4_instr(J, 32, I, 11011,      JAL)
  //   m4_instr(B, 32, I, 11000, 000, BEQ)
  //   m4_instr(S, 32, I, 01000, 000, SB)
  //   m4_instr(I, 32, I, 00100, 000, ADDI)
  //   m4_instr(If, 64, I, 00100, 101, 000000, SRLI)  // (imm[11:6] are used like funct7[6:1] and must be 000000)
  //   m4_instr(R, 32, I, 01100, 000, 0000000, ADD)
  //   m4_instr(R4, 32, F, 10000, rm, 10, FMADD.D)
  //   m4_instr(R2, 32, F, 10100, rm, 0101100, 00000, FSQRT.S)
  //   m4_instr(R2, 32, A, 01011, 010, 00010, 00000, LR.W)  // (5 bits for funct7 for all "A"-ext instrs)
  //   m4_instr(R, 32, A, 01011, 010, 00011, SC.W)          //   "
  // This defines assembler macros as follows. Fields are ordered rd, rs1, rs2, imm:
  //   I: m4_asm_ADDI(r4, r1, 0),
  //   R: m4_asm_ADD(r4, r1, r2),
  //   R2: m4_asm_FSQRT.S(r4, r1, 000),  // rm == 000
  //   R4: m4_asm_FMADD.S(r4, r1, r2, r3, 000),  // rm == 000
  //   S: m4_asm_SW(r1, r2, 100),  // Store r13 into [r10] + 4
  //   B: m4_asm_BLT(r1, r2, 1000), // Branch if r1 < r2 to PC + 13'b1000 (where lsb = 0)
  //   For "A"-extension instructions, an additional final arg is REQUIRED to provide 2 binary bits for aq and rl.
  // Macro definitions include 2 parts:
  //   o Hardware definitions: m4_instr_<mnemonic>($@)
  //   o Assembler definition of m4_asm_<MNEMONIC>: m4_define(['m4_asm_<MNEMONIC>'], ['m4_asm_instr_str(...)'])
  m4_define(['m4_instrI'], ['m4_instr_funct3($@)m4_define(['m4_asm_$5'],
       ['m4_asm_instr_str(I, ['$5'], $']['@){12'b']m4_arg(3)[', m4_asm_reg(']m4_arg(2)['), $5_INSTR_FUNCT3, m4_asm_reg(']m4_arg(1)['), $5_INSTR_OPCODE}'])'])
  m4_define(['m4_instrIf'], ['m4_instr_funct7($@, ['$6'], m4_len($5))m4_define(['m4_asm_$6'],
       ['m4_asm_instr_str(I, ['$6'], $']['@){['$6_INSTR_FUNCT']m4_len($5)[', ']m4_eval(12-m4_len($5))'b']m4_arg(3)[', m4_asm_reg(']m4_arg(2)['), $6_INSTR_FUNCT3, m4_asm_reg(']m4_arg(1)['), $6_INSTR_OPCODE}'])'])
  m4_define(['m4_instrR'], ['m4_instr_funct7($@, ['$6'], m4_ifelse($2, ['A'], 5, 7))m4_define(['m4_asm_$6'],
       ['m4_asm_instr_str(R, ['$6'], $']['@){m4_ifelse($2, ['A'], ['$6_INSTR_FUNCT5, ']']m4_arg(2)['[''], ['$6_INSTR_FUNCT7']), m4_asm_reg(']m4_arg(3)['), m4_asm_reg(']m4_arg(2)['), ']m4_asm_funct3(['$6'], ['$4'])[', m4_asm_reg(']m4_arg(1)['), $6_INSTR_OPCODE}'])'])
  m4_define(['m4_instrR2'], ['m4_instr_funct7($@, 7, ['$6'])m4_define(['m4_asm_$7'],
       ['m4_asm_instr_str(R, ['$7'], $']['@){m4_ifelse($2, ['A'], ['$7_INSTR_FUNCT5, ']']m4_arg(2)['[''], ['$7_INSTR_FUNCT7']), 5'b$6, m4_asm_reg(']m4_arg(2)['), ']m4_asm_funct3(['$7'], ['$4'])[', m4_asm_reg(']m4_arg(1)['), $7_INSTR_OPCODE}'])'])
  m4_define(['m4_instrR4'], ['m4_instr_funct2($@)m4_define(['m4_asm_$6'],
       ['m4_asm_instr_str(R, ['$6'], $']['@){m4_asm_reg(']m4_arg(4)['), $6_INSTR_FUNCT2, m4_asm_reg(']m4_arg(3)['), m4_asm_reg(']m4_arg(2)['), ']m4_asm_funct3(['$6'], ['$4'])[', m4_asm_reg(']m4_arg(1)['), $6_INSTR_OPCODE}'])'])
  m4_define(['m4_instrS'], ['m4_instr_funct3($@, ['no_dest'])m4_define(['m4_asm_$5'],
       ['m4_asm_instr_str(S, ['$5'], $']['@){m4_asm_imm_field(']m4_arg(3)[', 12, 11, 5), m4_asm_reg(']m4_arg(2)['), m4_asm_reg(']m4_arg(1)['), ']m4_asm_funct3(['$5'], ['$4'])[', m4_asm_imm_field(']m4_arg(3)[', 12, 4, 0), $5_INSTR_OPCODE}'])'])
  m4_define(['m4_instrB'], ['m4_instr_funct3($@, ['no_dest'])m4_define(['m4_asm_$5'],
       ['m4_asm_instr_str(B, ['$5'], $']['@){m4_asm_imm_field(']m4_arg(3)[', 13, 12, 12), m4_asm_imm_field(']m4_arg(3)[', 13, 10, 5), m4_asm_reg(']m4_arg(2)['), m4_asm_reg(']m4_arg(1)['), ']m4_asm_funct3(['$5'], ['$4'])[', m4_asm_imm_field(']m4_arg(3)[', 13, 4, 1), m4_asm_imm_field(']m4_arg(3)[', 13, 11, 11), $5_INSTR_OPCODE}'])'])
  m4_define(['m4_instrU'], ['m4_instr_no_func($@)m4_define(['m4_asm_$4'],
       ['m4_asm_instr_str(U, ['$4'], $']['@){m4_asm_imm_field(']m4_arg(2)[', 20, 19, 0), m4_asm_reg(']m4_arg(1)['), $4_INSTR_OPCODE}'])'])
  m4_define(['m4_instrJ'], ['m4_instr_no_func($@)m4_define(['m4_asm_$4'],
       ['m4_asm_instr_str(J, ['$4'], $']['@){m4_asm_imm_field(']m4_arg(2)[', 20, 19, 19), m4_asm_imm_field(']m4_arg(2)[', 20, 9, 0), m4_asm_imm_field(']m4_arg(2)[', 20, 10, 10), m4_asm_imm_field(']m4_arg(2)[', 20, 18, 11), m4_asm_reg(']m4_arg(1)['), $4_INSTR_OPCODE}'])'])
  m4_define(['m4_instr_'], ['m4_instr_no_func($@)'])

  // For each instruction type.
  // Declare localparam[31:0] INSTR_TYPE_X_MASK, initialized to 0 that will be given a 1 bit for each op5 value of its type.
  m4_define(['m4_instr_types_args'], ['I, R, R2, R4, S, B, J, U, _'])
  m4_instr_types(m4_instr_types_args)


  // Instruction fields (User ISA Manual 2.2, Fig. 2.2)
  m4_define_fields(['M4_INSTR'], 32, FUNCT7, 25, RS2, 20, RS1, 15, FUNCT3, 12, RD, 7, OP5, 2, OP2, 0)


  //=========
  // Specifically for assembler.

  // An 20-bit immediate binary zero string.
  m4_define(['m4_asm_imm_zero'], ['00000000000000000000'])
  // Zero-extend to n bits. E.g. m4_asm_zero_ext(1001, 7) => 0001001
  m4_define(['m4_asm_zero_ext'], ['m4_substr(m4_asm_imm_zero, 0, m4_eval($2 - m4_len($1)))$1'])
  // Extract bits from a binary immediate value.
  // m4_asm_imm_field(binary-imm, imm-length, max-bit, min-bit)
  // E.g. m4_asm_imm_field(101011, 17, 7, 3) => 5'b00101
  m4_define(['m4_asm_imm_field'], ['m4_eval($3 - $4 + 1)'b['']m4_substr(m4_asm_zero_ext($1, $2), m4_eval($2 - $3 - 1), m4_eval($3 - $4 + 1))'])
  // Register operand.
  m4_define(['m4_asm_reg'], ['m4_ifelse(m4_substr(['$1'], 0, 1), ['r'], [''], ['m4_errprint(['$1 passed to register field.'])'])5'd['']m4_substr(['$1'], 1)'])

  // For debug, a string for an asm instruction.
  m4_define(['m4_asm_mem_expr'], [''])
  // m4_asm_instr_str(<type>, <mnemonic>, <m4_asm-args>)
  m4_define(['m4_asm_instr_str'], ['m4_pushdef(['m4_str'], ['($1) $2 m4_shift(m4_shift($@))'])m4_define(['m4_asm_mem_expr'],
                                                   m4_dquote(m4_asm_mem_expr[' "']m4_str['']m4_substr(['                                        '], m4_len(m4_quote(m4_str)))['", ']))m4_popdef(['m4_str'])'])
  // Assemble an instruction.
  // m4_asm(FOO, ...) defines m4_inst# as m4_asm_FOO(...), counts instructions in M4_NUM_INSTRS ,and outputs a comment.
  m4_define(['m4_asm'], ['m4_define(['m4_instr']M4_NUM_INSTRS, ['m4_asm_$1(m4_shift($@))'])['/']['/ Inst #']M4_NUM_INSTRS: $@m4_define(['M4_NUM_INSTRS'], m4_eval(M4_NUM_INSTRS + 1))'])

  //=========
'])
