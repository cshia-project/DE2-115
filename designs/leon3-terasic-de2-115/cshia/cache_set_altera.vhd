library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library CSHIA;
use CSHIA.cache_pkg.all;

entity cache_set is
	port (
		clk					: in	std_logic;
		my_number			: in	natural;
		data				: in	T_WORD;
		address				: in	T_ADDRESS_VECTOR;
		we					: in	std_logic;
		q					: out	T_WORD
	);
end cache_set;

architecture rtl of cache_set is
	signal ram_block : T_RAM;
begin
	process (clk)
		begin
		if (clk'event and clk = '1') then
			if (we = '1') then
				ram_block(address) <= data;
			end if;
		end if;
	end process;
	q <= ram_block(address);

end rtl;

