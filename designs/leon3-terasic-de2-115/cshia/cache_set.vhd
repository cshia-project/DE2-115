library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library CSHIA;
use CSHIA.cache_pkg.all;
use std.textio.all;

entity cache_set is
	port (
		clock1,
		clock2				: in	std_logic;
		my_number			: in	natural;
		data				: in	T_WORD;
		write_address		: in	T_ADDRESS_VECTOR;
		read_address		: in	T_ADDRESS_VECTOR;
		we					: in	std_logic;
		q					: out	T_WORD
	);
end cache_set;

architecture rtl of cache_set is
	signal ram_block : T_RAM;
begin
	process (clock1)
	variable l : line;
		begin
		if (clock1'event and clock1 = '1') then
			if (we = '1') then
				ram_block(write_address) <= data;
			end if;
		end if;
	end process;
	process (clock2)
	variable l : line;
		begin
			if (clock2'event and clock2 = '1') then
				q <= ram_block(read_address);
			end if;
	end process;
end rtl;

