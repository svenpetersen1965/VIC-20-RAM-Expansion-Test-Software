# VIC-20-RAM-Expansion-Test-Software
A Test Software for RAM Expansions of the Commodore VIC-20
<img src="https://github.com/svenpetersen1965/VIC-20-RAM-Expansion-Test-Software/blob/main/Rev.%200.0/pictures/test_running.png" width="600" alt="VIC20 RAM Expansion Test">

This software tests every RAM expansion block connected to the RAM1..3 Chip selects or BLK1, 2, 3 and 5. ALso cross linking is checked, which means, that writing to a tested RAM block could corrupt the content of other RAM blocks.

There are two versions, a *.d64 disk image and a BIN-file, which can be programmed into an EPROM cartridge for the VIC-20, such as the <a href="https://github.com/svenpetersen1965/Commodore-VIC-20-Super-Expander-II">Super Expander II</a> or the Hyper Expander.

