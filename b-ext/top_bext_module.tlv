\m4_TLV_version 1d: tl-x.org
   
// 1) CLZ (count leading zeros)   
// e.g. m4+clz_final(|pipe, /clz_stage, 32, 0, 1, $input, $output)
\TLV clz_final(/_top, /_clz_stage, #_varbits, #_stage, #_stageinc, $_data_value, $_clz_output)
   m4_pushdef(['m4_clz_stage'], m4_strip_prefix(/_clz_stage))
   m4_ifelse_block(m4_eval(#_stage == 0 ), 1, ['
   /_clz_stage['']#_stageinc[''][((#_varbits / 2) - 1) : 0]
      $clz_stage['']#_stageinc[['']#_stageinc : 0] = (/_top$_data_value[((2*(#m4_clz_stage['']#_stageinc)) + 1) : (2*(#m4_clz_stage['']#_stageinc))] == 2'b00) ? 2'b10 :
                          (/_top$_data_value[((2*(#m4_clz_stage['']#_stageinc)) + 1) : (2*(#m4_clz_stage['']#_stageinc))] == 2'b01) ? 2'b01 : 2'b00;
   m4+clz_final(/_top, /_clz_stage, m4_eval(#_varbits / 2), m4_eval(#_stage + 1),m4_eval(#_stageinc + 1), $clz_stage['']#_stageinc, $_clz_output)
   '],['
   /_clz_stage['']#_stageinc[''][((#_varbits / 2) - 1) : 0]
      $clz_stage['']#_stageinc[['']#_stageinc : 0] = (/_top/_clz_stage['']#_stage[(2*(#m4_clz_stage['']#_stageinc)) + 1]$_data_value[#_stage] == 1'b1  && /_top/_clz_stage['']#_stage[2*(#m4_clz_stage['']#_stageinc)]$_data_value[#_stage] == 1'b1 ) ? {1'b1,#_stageinc'b0} :
                           (/_top/_clz_stage['']#_stage[(2*(#m4_clz_stage['']#_stageinc)) + 1]$_data_value[#_stage] == 1'b0) ? {1'b0,/_top/_clz_stage['']#_stage[(2*(#m4_clz_stage['']#_stageinc)) + 1]$_data_value[#_stage : 0]} : {2'b01,/_top/_clz_stage['']#_stage[(2*(#m4_clz_stage['']#_stageinc)) ]$_data_value[#_stage - 1 : 0]};
   m4_ifelse_block(m4_eval(#_varbits > 2), 1, ['
   m4+clz_final(/_top, /_clz_stage, m4_eval(#_varbits / 2), m4_eval(#_stage + 1),m4_eval(#_stageinc + 1), $clz_stage['']#_stageinc, $_clz_output)
   '], ['
   $_clz_output[['']#_stageinc : 0] = /_clz_stage['']#_stageinc[0]$clz_stage['']#_stageinc[['']#_stageinc : 0];
   '])
   '])
   m4_popdef(['m4_clz_stage'])
   
// 2) CTZ (count trailing zeros)   
// e.g. m4+ctz_final(|pipe, /ctz_stage, /reverse, 32, 0, 1, $input, $output)
\TLV ctz_final(/_top, /_ctz_stage, /_reverse, #_varbits, #_stage, #_stageinc, $_data_value, $_ctz_output) 
   m4_pushdef(['m4_ctz_stage'], m4_strip_prefix(/_ctz_stage))
   /_ctz_stage['']0[#_varbits - 1 : 0]
      $reg_val = /_top$_data_value[31 - #m4_ctz_stage['']0];
   /_reverse
      $reverse_ctz[#_varbits - 1 : 0] = /_top/_ctz_stage['']0[*]$reg_val;
   m4+clz_final(/_top,/_ctz_stage,#_varbits,#_stage,#_stageinc,/_reverse$reverse_ctz, $_ctz_output)
   m4_popdef(['m4_ctz_stage'])
   
// 3) popcnt (count no. of '1' in the input)
// e.g. m4+popcnt(|pipe, /pop_stage, $input, $output, 32)
\TLV popcnt(/_top, /_pop_bit, $_input, $_output, #_XLEN)
   m4_pushdef(['m4_pop_bit'], m4_strip_prefix(/_pop_bit))
   /_pop_bit[['']#_XLEN -1 : 0]
      $pop_temp[#_XLEN -1 : 0] = { {(#_XLEN - 1){1'b0}} ,/_top$_input[#m4_pop_bit]};
   m4+redux($_output[\$clog2(#_XLEN) : 0], /_pop_bit, m4_eval(#_XLEN - 1) , 0, $pop_temp, '0, +)
   m4_popdef(['m4_pop_bit'])
   
// 4) Logic-with-negate instructions (andn, orn, xnor)
// RS1 = input1  and RS2 = input2  and RD=output
\TLV andn($_input1,$_input2,$_output)
   $_output = $_input1 & ~($_input2);
\TLV orn($_input1,$_input2,$_output)   
   $_output = $_input1 | ~($_input2);
\TLV xnor($_input1,$_input2,$_output)
   $_output = $_input1 ^ ~($_input2);

// 7) Pack two words in one register (pack, packu, packh)
//The "pack" instruction packs the XLEN/2-bit lower halves of 
//rs1 and rs2 into rd, with rs1 in the lower half and rs2 in 
//the upper half.
// RS1 = input1  and RS2 = input2  and RD=output
\TLV pack($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = { $_input2[#_XLEN - 1 : (#_XLEN / 2)], $_input1[(#_XLEN / 2) - 1 : 0]};
// 8) The "packu" instruction packs the upper halves of rs1 and rs2 into rd.
\TLV packu($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = { $_input2[#_XLEN - 1 : (#_XLEN / 2)], $_input1[#_XLEN - 1 : (#_XLEN / 2)]};
// 9) The "packh" instruction packs the LSB bytes of rs1 and rs2 into the 16 LSB
// bits of rd, zero extending the rest of rd.
\TLV packh($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = { {(m4_eval(#_XLEN / 2) - 1){1'b0}} ,$_input2[(#_XLEN / 4) - 1 : 0], $_input1[(#_XLEN / 4) - 1 : 0]};
   
// 8.) Min/max instructions (min, max, minu, maxu)
// Returns max value of the two registers either signed or unsigned(u) operation.
// RS1 = input1  and RS2 = input2  and RD=output
\TLV minu($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = ($_input1 < $_input2) ? $_input1 : $_input2;
\TLV maxu($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = ($_input1 < $_input2) ? $_input2 : $_input1;
\TLV min($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = (($_input1[#_XLEN - 1] == 1'b1) && ($_input2[#_XLEN - 1] == 1'b0)) ? $_input1 :
                              (($_input1[#_XLEN - 1] == 1'b0) && ($_input2[#_XLEN - 1] == 1'b1)) ? $_input2 :
                              (($_input1[#_XLEN - 1] == 1'b0) && ($_input2[#_XLEN - 1] == 1'b0) && ($_input1 < $_input2)) ? $_input1 :
                              (($_input1[#_XLEN - 1] == 1'b0) && ($_input2[#_XLEN - 1] == 1'b0) && ($_input1 > $_input2)) ? $_input2 :
                              (($_input1[#_XLEN - 1] == 1'b1) && ($_input2[#_XLEN - 1] == 1'b1) && ($_input1 < $_input2)) ? $_input2 : $_input1;
\TLV max($_input1,$_input2,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = (($_input1[#_XLEN - 1] == 1'b1) && ($_input2[#_XLEN - 1] == 1'b0)) ? $_input2 :
                              (($_input1[#_XLEN - 1] == 1'b0) && ($_input2[#_XLEN - 1] == 1'b1)) ? $_input1 :
                              (($_input1[#_XLEN - 1] == 1'b0) && ($_input2[#_XLEN - 1] == 1'b0) && ($_input1 < $_input2)) ? $_input2 :
                              (($_input1[#_XLEN - 1] == 1'b0) && ($_input2[#_XLEN - 1] == 1'b0) && ($_input1 > $_input2)) ? $_input1 :
                              (($_input1[#_XLEN - 1] == 1'b1) && ($_input2[#_XLEN - 1] == 1'b1) && ($_input1 < $_input2)) ? $_input1 : $_input2;  

// 14) Single-bit instructions (sbset, sbclr, sbinv, sbext)   
// single-bit instructions sbset (set), sbclr (clear), sbinv (invert), and sbext (extract),
//and their immediate-variants,   
// RS1 = input1(data)  and RS2 = input2(sft_amt)  and RD = output
//Set bit to'1' at location rs1[rs2].
\TLV sbset($_input1,$_input2,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ($_input1 | (1 << ($_input2 & (#_XLEN - 1))));
// 15) Clear bit to '0' at location rs1[rs2].
\TLV sbclr($_input1,$_input2,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ($_input1 & ~(1 << ($_input2 & (#_XLEN - 1))));
// 16) reverse the bit at location rs1[rs2].
\TLV sbinv($_input1,$_input2,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ($_input1 ^ (1 << ($_input2 & (#_XLEN - 1))));
// 17) Extract the status of bit at loaction rs1[rs2].
\TLV sbext($_input1,$_input2,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ( 1 & ($_input1 >> ($_input2 & (#_XLEN - 1))));
// 18) RS1 = input1(data)  and imm = input2(sft_amt)  and RD = output
\TLV sbseti($_input1,$_imm,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ($_input1 | (1 << ($_imm[#_XLEN - 1 : 0] & (#_XLEN - 1))));
// 19) RS1 = input1(data)  and imm = input2(sft_amt)  and RD = output
\TLV sbclri($_input1,$_imm,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = ($_input1 & ~(1 << ($_imm[#_XLEN - 1 : 0] & (#_XLEN - 1))));
// 20) RS1 = input1(data)  and imm = input2(sft_amt)  and RD = output
\TLV sbinvi($_input1,$_imm,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ($_input1 ^ (1 << ($_imm[#_XLEN - 1 : 0] & (#_XLEN - 1))));
// 21) RS1 = input1(data)  and imm = input2(sft_amt)  and RD = output
\TLV sbexti($_input1,$_imm,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ( 1 & ($_input1 >> ($_imm[#_XLEN - 1 : 0] & (#_XLEN - 1))));
   
// 22) Shift Ones (Left/Right) (slo, sloi, sro, sroi)
//Similar to shift-logical operations, except instead of shifting in zeros, they shift in ones
// RS1 = input1(data)  and RS2 = input2(sft_amt)  and RD = output
\TLV slo($_input1,$_input2,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ~(( ~($_input1) << ($_input2 & (#_XLEN - 1))));
// 23) RS1 = input1(data)  and RS2 = input2(sft_amt)  and RD = output   
\TLV sro($_input1,$_input2,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ~(( ~($_input1) >> ($_input2 & (#_XLEN - 1))));
// 24) RS1 = input1(data)  and imm = input2(sft_amt)  and RD = output
\TLV sloi($_input1,$_imm,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ~(( ~($_input1) << ($_imm[#_XLEN - 1 : 0] & (#_XLEN - 1))));
// 25) RS1 = input1(data)  and RS2 = input2(sft_amt)  and RD = output
\TLV sroi($_input1,$_imm,$_output,#_XLEN)   
   $_output[#_XLEN - 1 : 0] = ~(( ~($_input1) >> ($_imm[#_XLEN - 1 : 0] & (#_XLEN - 1))));
   
// 26) ROTL (rotate left)
// e.g. m4+rorl_final(32, 1, $input, $sftamt, $output, 31, 0)
\TLV rorl_final(#_varbits,#_stage,$_reg_value,$_sft_amt,$_rotl,#_max,#_min) 
   \always_comb
      $['']$_rotl['']#_stage[#_max : #_min] = 0;
      for (int i = #_min; i <= #_max; i++)
      begin
         $_rotl['']#_stage[i] = ($_sft_amt[#_stage - 1] == 0) ?
              $_reg_value[i] : (i >= 0 && i < (2**(#_stage - 1))) ?
              $_reg_value[(i+((#_max + 1) - (2**(#_stage - 1))))] :
              $_reg_value[(i-(2**(#_stage - 1)))]; end
   m4_ifelse_block(m4_eval(#_varbits > 2), 1, ['
   m4+rorl_final(m4_eval(#_varbits / 2), m4_eval(#_stage + 1), $_rotl['']#_stage, $_sft_amt, $_rotl, #_max, #_min)
   '], ['
   $_rotl[#_max : #_min] = $_rotl['']#_stage[#_max : #_min];
   '])
   
// 27) ROTR (rotate right)
// e.g. m4+rorr_final(32, 1, $input, $sftamt, $output, 31, 0)
\TLV rorr_final(#_varbits,#_stage,$_reg_value,$_sft_amt,$_rotr,#_max,#_min) 
   \always_comb
      $['']$_rotr['']#_stage[#_max : #_min] = 0;
      for (int i = #_min; i <= #_max; i++)
      begin
         $_rotr['']#_stage[i] = ($_sft_amt[#_stage - 1] == 0) ?
              $_reg_value[i] : (i <= (#_max) && i > (#_max - (2**(#_stage - 1)))) ?
              $_reg_value[(i-((#_max + 1) - (2**(#_stage - 1))))] :
              $_reg_value[(i+(2**(#_stage - 1)))]; end
   m4_ifelse_block(m4_eval(#_varbits > 2), 1, ['
   m4+rorr_final(m4_eval(#_varbits / 2), m4_eval(#_stage + 1), $_rotr['']#_stage, $_sft_amt, $_rotr, #_max, #_min)
   '], ['
   $_rotr[#_max : #_min] = $_rotr['']#_stage[#_max : #_min];
   '])
   
// 28) BREV (bit-wise reversal generalized)
// e.g. m4+brev_final(|pipe, /brev_stage, 32, 32, 0, 1, $input, $sft_amt, $output)
\TLV brev_final(/_top,/_brev_stage,#_constbits,#_varbits,#_stage,#_stageinc,$_data_value,$sft_amt,$resultq) 
   m4_pushdef(['m4_brev_stage'], m4_strip_prefix(/_brev_stage))
   m4_ifelse_block(m4_eval(#_stage == 0 ), 1, ['
   /_brev_stage[''][(#_constbits - 1) : 0]
      $brev_stage_val['']#_stageinc = (/_top$sft_amt[#_stage] == 1'b0)  ? /_top$_data_value[#m4_brev_stage] :
          ((#m4_brev_stage % (2 ** #_stageinc)) == 0 ) ? /_top$_data_value[#m4_brev_stage + (2 ** #_stage)] :
          /_top$_data_value[#m4_brev_stage - (2 ** #_stage)];
   m4+brev_final(/_top,/_brev_stage,#_constbits,m4_eval(#_varbits / 2), m4_eval(#_stage + 1),m4_eval(#_stageinc + 1),$brev_stage_val['']#_stageinc,$sft_amt,$resultq)
   '],['
   /_brev_stage[''][(#_constbits - 1) : 0]
      $brev_stage_val['']#_stageinc = (/_top$sft_amt[#_stage] == 1'b0)  ? /_top/_brev_stage[['']#m4_brev_stage]$_data_value :
          ( ((['']#m4_brev_stage % (2 ** #_stageinc)) >= 0) && ((['']#m4_brev_stage % (2 ** #_stageinc)) < (2 ** #_stage) )) ? /_top/_brev_stage[['']#m4_brev_stage + (2 ** #_stage)]$_data_value:
          /_top/_brev_stage[#m4_brev_stage - (2 ** #_stage)]$_data_value;
   m4_ifelse_block(m4_eval(#_varbits > 2), 1, ['
   m4+brev_final(/_top,/_brev_stage,#_constbits,m4_eval(#_varbits / 2), m4_eval(#_stage + 1),m4_eval(#_stageinc + 1),$brev_stage_val['']#_stageinc,$sft_amt,$resultq)
   '], ['
   $resultq[(#_constbits - 1) : 0] = /_brev_stage[*]$brev_stage_val['']#_stageinc;
   '])
   '])
   m4_popdef(['m4_brev_stage'])
   
// 29) This marco is inheritated from RISC-V bitmanip draft verilog module.
// This module contains bext, bdep, gorc, grev, shfl, and unshfl instruction
\TLV bext_dep(#_number,/_top ,#_xlen, #_is_grev, #_is_shfl, #_latency, $_clk, $_reset, $_din_valid, $_din_ready, $_din_rs1, $_din_rs2, $_din_insn3, $_din_insn13, $_din_insn14, $_din_insn29, $_din_insn30, $_dout_valid, $_dout_ready, $_dout_rd)
   \SV_plus
      rvb_bextdep#(#_xlen,#_is_grev,#_is_shfl,#_latency)
      rvb_bextdep#_number($_clk, $_reset, /_top$_din_valid, $['']$_din_ready, /_top$_din_rs1[#_xlen - 1 : 0], /_top$_din_rs2[#_xlen - 1 : 0], /_top$_din_insn3, /_top$_din_insn13, /_top$_din_insn14, /_top$_din_insn29, /_top$_din_insn30, $['']$_dout_valid, /_top$_dout_ready, $['']$_dout_rd);

// 30) Bit-Field Place
// The bit field place(bfp) instruction places up to XLEN/2 LSB bits from rs2 into the value in rs1.
// The upper bits of rs2 control the length of the bit field and target position .The layout of rs2 is
// chosen in a way that makes it possible to construct rs2 easily using pack[h] instructions and/or
// andi/lui.
// TODO. Only supported for 32 and 64 bits at present
\TLV bfp($_input1,$_input2,$_output,#_XLEN)
   $cfg_temp1[ (#_XLEN / 2) - 1 : 0] = $_input2[ (#_XLEN - 1) : (#_XLEN / 2)];
   $cfg[ (#_XLEN / 2) - 1 : 0] = (($cfg_temp1 >> 30) == 2'b10) ? ($cfg_temp1 >> 16) : $cfg_temp1;
   $len[4:0] = ($cfg >> 8) & ((#_XLEN / 2) - 1);
   $off[5:0] = $cfg & (#_XLEN - 1);
   $mask[(#_XLEN - 1) : 0] = ((1 << ($len[4:0] + 1)) - 1) << $off;
   $data[ (#_XLEN ) - 1 : 0] = $_input2 << $off;
   $_output[ (#_XLEN - 1) : 0] = ($data & $mask) | ($_input1 & ~$mask);
   
// 31) Carry-Less Multiply (clmul,  clmulh,  clmulr)
// This marco is inheritated from RISC-V bitmanip draft verilog module.
// This module contains clmul, clmulh and clmulr instruction and
// takes 4 clock cycles to calculate results.
\TLV clmul(#_number,/_top ,#_xlen, $_clk, $_reset, $_din_valid, $_din_ready, $_din_rs1, $_din_rs2, $_din_insn3, $_din_insn12, $_din_insn13, $_dout_valid, $_dout_ready, $_dout_rd)
   \SV_plus
      rvb_clmul#(#_xlen)
      rvb_clmul#_number($_clk, $_reset, /_top$_din_valid, $['']$_din_ready, /_top$_din_rs1[#_xlen - 1 : 0], /_top$_din_rs2[#_xlen - 1 : 0], /_top$_din_insn3, /_top$_din_insn12, /_top$_din_insn13, $['']$_dout_valid, /_top$_dout_ready, $['']$_dout_rd);   
   
// 32) CRC Instructions (crc32.[bhwd],  crc32c.[bhwd])
// TODO. at present not clear how it works.
\TLV rvb_crc(#_number, /_top, #_xlen, $_clk, $_reset, $_din_valid, $_din_ready, $_din_rs1, $_din_insn20, $_din_insn21, $_din_insn23, $_dout_valid, $_dout_ready, $_dout_rd)
   \SV_plus
      rvb_crc#(#_xlen)
      rvb_crc#_number($_clk, $_reset, /_top$_din_valid, $['']$_din_ready, /_top$_din_rs1[#_xlen - 1 : 0], /_top$_din_insn20, /_top$_din_insn21, /_top$_din_insn23, $['']$_dout_valid, /_top$_dout_ready, $['']$_dout_rd);

// Ternary Bit-Manipulation Instructions
// 33) Conditional Mix (cmix)
\TLV cmix($_input1,$_input2,$_input3,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = (($_input1[#_XLEN - 1 : 0] & $_input2[#_XLEN - 1 : 0]) | ($_input3[#_XLEN - 1 : 0] & (~ $_input2[#_XLEN - 1 : 0])));

// 34) Conditional Move (cmov)
// The cmov instruction helps avoiding branches, which can lead to better performance, and
// helps with constant-time code as used in some cryptography applications.
\TLV cmov($_input1,$_input2,$_input3,$_output,#_XLEN)
   $_output[#_XLEN - 1 : 0] = (($_input2[#_XLEN - 1 : 0]) ? $_input1[#_XLEN - 1 : 0] : $_input3[#_XLEN - 1 : 0]);
   
// 35) Funnel Shift (fsl, fsr, fsri)
// This marco is inheritated from RISC-V bitmanip draft verilog module.
// This module contains sll, srl, sra, slo, sro, rol, ror, fsl, fsr,
// slliu.w, sbset, sbclr, sbinv, sbext and bfp instruction.
\TLV rvb_shifter(#_number, /_top, #_xlen, #_sop, #_bfp, $_clk, $_reset, $_din_valid, $_din_ready, $_din_rs1, $_din_rs2, $_din_rs3, $_din_insn3, $_din_insn13, $_din_insn14, $_din_insn26, $_din_insn27, $_din_insn29, $_din_insn30, $_dout_valid, $_dout_ready, $_dout_rd)
   \SV_plus
      rvb_shifter#(#_xlen,#_sop,#_bfp)
      rvb_shifter#_number($_clk, $_reset, /_top$_din_valid, $['']$_din_ready, /_top$_din_rs1[#_xlen - 1 : 0], /_top$_din_rs2[#_xlen - 1 : 0], /_top$_din_rs3[#_xlen - 1 : 0], /_top$_din_insn3, /_top$_din_insn13, /_top$_din_insn14, /_top$_din_insn26, /_top$_din_insn27, /_top$_din_insn29, /_top$_din_insn30, $['']$_dout_valid, /_top$_dout_ready, $['']$_dout_rd);

// 36) Population Count
// This marco is inheritated from RISC-V bitmanip draft verilog module.
// This module contains clz, ctz, pcnt, bmatflip, sext.b and sext.h instruction.
\TLV rvb_bitcnt(#_number, /_top, #_xlen, #_bmat, $_clk, $_reset, $_din_valid, $_din_ready, $_din_rs1, $_din_insn3, $_din_insn20, $_din_insn21, $_din_insn22, $_dout_valid, $_dout_ready, $_dout_rd)
   \SV_plus
      rvb_bitcnt#(#_xlen,#_bmat)
      rvb_bitcnt#_number($_clk, $_reset, /_top$_din_valid, $['']$_din_ready, /_top$_din_rs1[#_xlen - 1 : 0], /_top$_din_insn3, /_top$_din_insn20, /_top$_din_insn21, /_top$_din_insn22, $['']$_dout_valid, /_top$_dout_ready, $['']$_dout_rd);


\SV
// Include verilog and tlv files.
/* verilator lint_off CMPCONST */
/* verilator lint_off WIDTH */
/* verilator lint_off PINMISSING */
m4_include_lib(['https://raw.githubusercontent.com/stevehoover/tlv_lib/481188115b4338567df916460d462ca82401e211/fundamentals_lib.tlv'])
m4_sv_include_url(['https://raw.githubusercontent.com/riscv/riscv-bitmanip/master/verilog/rvb_bextdep/rvb_bextdep.v'])
m4_sv_include_url(['https://raw.githubusercontent.com/riscv/riscv-bitmanip/master/verilog/rvb_shifter/rvb_shifter.v'])
m4_sv_include_url(['https://raw.githubusercontent.com/riscv/riscv-bitmanip/master/verilog/rvb_clmul/rvb_clmul.v'])
m4_sv_include_url(['https://raw.githubusercontent.com/riscv/riscv-bitmanip/master/verilog/rvb_crc/rvb_crc.v'])
m4_sv_include_url(['https://raw.githubusercontent.com/vineetjain07/riscv-bitmanip/master/verilog/rvb_bitcnt/rvb_bitcnt.v'])
/* verilator lint_on CMPCONST */   
/* verilator lint_off WIDTH */   
