# RISC-V B-Extension Files

**NOTE: The B-extension is still WIP and yet NOT FROZEN in RISC-V specs.**

* This directory contains the topmodule and Marco's for Bit-Manipulation Instructions(BMI's) which are neccesary for performing BMI operations in Warp-V.
* Some of the verilog module are inherited from Clifford Wolf's draft implementation  as proposed in riscv-bitmanip draft version-0.92 [RISC-V Bitmanip (Bit Manipulation) Extension](https://github.com/riscv/riscv-bitmanip).

* `top_bext_module.tlv` contains TL-Verilog marcos which is inheriting/calling the BMI verilog modules in TL-V context and also contains standalone implementation of some BMI's in pure TL-V.
