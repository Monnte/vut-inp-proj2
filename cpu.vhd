-- cpu.vhd: Simple 8-bit CPU (BrainF*ck interpreter)
-- Copyright (C) 2020 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Peter Zdravecký (xzdrav00)
--

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
ENTITY cpu IS
    PORT (
        CLK : IN STD_LOGIC; -- hodinovy signal
        RESET : IN STD_LOGIC; -- aCURRENT_STATE_LOGIC_SYNCHROnni reset procesoru
        EN : IN STD_LOGIC; -- povoleni cinnosti procesoru

        -- CURRENT_STATE_LOGIC_SYNCHROnni pamet ROM
        CODE_ADDR : OUT STD_LOGIC_VECTOR(11 DOWNTO 0); -- adresa do pameti
        CODE_DATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
        CODE_EN : OUT STD_LOGIC; -- povoleni cinnosti             

        -- CURRENT_STATE_LOGIC_SYNCHROnni pamet RAM
        DATA_ADDR : OUT STD_LOGIC_VECTOR(9 DOWNTO 0); -- adresa do pameti
        DATA_WDATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
        DATA_RDATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
        DATA_WE : OUT STD_LOGIC; -- cteni (0) / zapis (1)
        DATA_EN : OUT STD_LOGIC; -- povoleni cinnosti 

        -- vstupni port
        IN_DATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
        IN_VLD : IN STD_LOGIC; -- data platna
        IN_REQ : OUT STD_LOGIC; -- pozadavek na vstup data

        -- vystupni port
        OUT_DATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- zapisovana data
        OUT_BUSY : IN STD_LOGIC; -- LCD je zaneprazdnen (1), nelze zapisovat
        OUT_WE : OUT STD_LOGIC -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
    );
END cpu;
-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
ARCHITECTURE behavioral OF cpu IS

    TYPE STATE_TYPE IS (
        state_reset,
        state_start,
        state_decode,

        state_move_address_left, --  <  0x3E
        state_move_address_right, --  >  0x3C

        state_ram_data_inc_read, --  +  0x2B
        state_ram_data_inc_set,
        state_ram_data_inc_write,

        state_ram_data_dec_read, --  -  0x2D
        state_ram_data_dec_set,
        state_ram_data_dec_write,

        state_loop_start_init, --  [  0x5B
        state_loop_start_0,
        state_loop_start_1,
        state_loop_skip_instruction,

        state_loop_end_init, --  ]  0x5D
        state_loop_end_0,
        state_loop_end_1,

        state_read_init, --  ,  0x2E
        state_read_set,

        state_write_init, --  .  0x2C
        state_write_set,

        state_others,
        state_null -- null 0x00
    );

    SIGNAL STATE : STATE_TYPE := state_start;
    SIGNAL NEXSTATE : STATE_TYPE;

    SIGNAL DATA : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL MX_SLC : STD_LOGIC_VECTOR(1 DOWNTO 0);

    SIGNAL PC : STD_LOGIC_VECTOR(11 DOWNTO 0);
    SIGNAL PC_INC : STD_LOGIC;
    SIGNAL PC_DEC : STD_LOGIC;

    TYPE RAS IS ARRAY (0 TO 15) OF STD_LOGIC_VECTOR(11 DOWNTO 0);

    SIGNAL RAS_REG : RAS := (OTHERS => (OTHERS => '0'));
    SIGNAL RAS_POP : STD_LOGIC;
    SIGNAL RAS_PUSH : STD_LOGIC;
    SIGNAL RAS_TOP : STD_LOGIC;

    SIGNAL CNT : STD_LOGIC_VECTOR(11 DOWNTO 0);
    SIGNAL CNT_INC : STD_LOGIC;
    SIGNAL CNT_DEC : STD_LOGIC;

    SIGNAL PTR : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL PTR_INC : STD_LOGIC;
    SIGNAL PTR_DEC : STD_LOGIC;

BEGIN
    OUT_DATA <= DATA_RDATA;

    --PC REGISTER LOGIC--
    PC_CNTRL : PROCESS (CLK, RESET, PC_INC, PC_DEC, RAS_TOP, PC)
    BEGIN
        IF (RESET = '1') THEN
            PC <= (OTHERS => '0');
        ELSIF (rising_edge(CLK)) THEN
            IF (PC_INC = '1') THEN
                PC <= PC + 1;
            ELSIF (PC_DEC = '1') THEN
                PC <= PC - 1;
            ELSIF (RAS_TOP = '1') THEN
                PC <= RAS_REG(0);
            END IF;
        END IF;

        CODE_ADDR <= PC;
    END PROCESS PC_CNTRL;

    RAS_CNTRL : PROCESS (CLK, RESET, RAS_POP, RAS_PUSH)
    BEGIN
        IF (RESET = '1') THEN

            FOR i IN 15 DOWNTO 0 LOOP
                RAS_REG(i) <= (OTHERS => '0');
            END LOOP;

        ELSIF (rising_edge(CLK)) THEN
            IF (RAS_PUSH = '1') THEN

                FOR i IN 15 DOWNTO 1 LOOP
                    RAS_REG(i) <= RAS_REG(i - 1);
                END LOOP;
                RAS_REG(0) <= PC;

            ELSIF (RAS_POP = '1') THEN

                RAS_REG(0) <= (OTHERS => '0');
                FOR i IN 15 DOWNTO 1 LOOP
                    RAS_REG(i - 1) <= RAS_REG(i);
                END LOOP;

            END IF;
        END IF;
    END PROCESS RAS_CNTRL;

    --PTR REGISTER LOGIC--
    PTR_CNTRL : PROCESS (CLK, RESET, PTR_INC, PTR_DEC, PTR)
    BEGIN

        IF (RESET = '1') THEN
            PTR <= (OTHERS => '0');
        ELSIF (rising_edge(CLK)) THEN
            IF (PTR_INC = '1') THEN
                PTR <= PTR + 1;
            ELSIF (PTR_DEC = '1') THEN
                PTR <= PTR - 1;
            END IF;
        END IF;

        DATA_ADDR <= PTR;
    END PROCESS PTR_CNTRL;

    --COUNTER REGISTER LOGIC--
    CNT_CNTRL : PROCESS (CLK, RESET, CNT_INC, CNT_DEC)
    BEGIN
        IF (RESET = '1') THEN
            CNT <= (OTHERS => '0');
        ELSIF (rising_edge(CLK)) THEN
            IF (CNT_INC = '1') THEN
                CNT <= CNT + 1;
            ELSIF (CNT_DEC = '1') THEN
                CNT <= CNT - 1;
            END IF;
        END IF;
    END PROCESS CNT_CNTRL;

    --DATA WRITE REGISTER LOGIC--
    HANDLE_DATA : PROCESS (CLK, RESET, MX_SLC, DATA)
    BEGIN
        IF (RESET = '1') THEN
            DATA <= (OTHERS => '0');
        ELSIF (rising_edge(CLK)) THEN
            CASE MX_SLC IS
                WHEN "00" => DATA <= IN_DATA;
                WHEN "01" => DATA <= DATA_RDATA + 1;
                WHEN "10" => DATA <= DATA_RDATA - 1;
                WHEN OTHERS => DATA <= DATA_RDATA;
            END CASE;
        END IF;
        DATA_WDATA <= DATA;
    END PROCESS HANDLE_DATA;

    -- FSM LOGIC --
    CURRENT_STATE_LOGIC_SYNCHRO : PROCESS (CLK, RESET, EN)
    BEGIN
        IF (RESET = '1') THEN
            STATE <= state_reset;
        ELSIF (rising_edge(CLK)) THEN
            IF (EN = '1') THEN
                STATE <= NEXSTATE;
            END IF;
        END IF;
    END PROCESS CURRENT_STATE_LOGIC_SYNCHRO;
    NEXT_STATE_LOGIC : PROCESS (DATA, STATE, IN_VLD, OUT_BUSY, CODE_DATA, CNT, IN_VLD, DATA_RDATA)
    BEGIN

        -- INICIALIZTAION -- OF VARIABLES TO PREVENT UNDEFINED STATE
        MX_SLC <= (OTHERS => '0');

        PC_INC <= '0';
        PC_DEC <= '0';
        RAS_POP <= '0';
        RAS_PUSH <= '0';
        RAS_TOP <= '0';

        CNT_INC <= '0';
        CNT_DEC <= '0';

        PTR_INC <= '0';
        PTR_DEC <= '0';

        IN_REQ <= '0';

        CODE_EN <= '0';
        DATA_EN <= '0';

        OUT_WE <= '0';
        DATA_WE <= '0';
        -- END OF INICIALIZTAION --

        NEXSTATE <= STATE;
        CASE STATE IS
            WHEN state_reset => -- RESET
                NEXSTATE <= state_start;

            WHEN state_start => -- Enable code en to update code data
                CODE_EN <= '1';
                NEXSTATE <= state_decode;

            WHEN state_decode => -- Decoding code data
                CASE CODE_DATA IS
                    WHEN x"3e" => NEXSTATE <= state_move_address_right; -- >
                    WHEN x"3c" => NEXSTATE <= state_move_address_left; -- <
                    WHEN x"2b" => NEXSTATE <= state_ram_data_inc_read; -- +
                    WHEN x"2d" => NEXSTATE <= state_ram_data_dec_read; -- -
                    WHEN x"5b" => NEXSTATE <= state_loop_start_init; -- [
                    WHEN x"5d" => NEXSTATE <= state_loop_end_init; -- ]
                    WHEN x"2e" => NEXSTATE <= state_write_init; -- .
                    WHEN x"2c" => NEXSTATE <= state_read_init; -- ,
                    WHEN x"00" => NEXSTATE <= state_null; -- null
                    WHEN OTHERS => NEXSTATE <= state_others;
                END CASE;

                -- Instruction < 0x3C --
            WHEN state_move_address_left =>
                PTR_DEC <= '1';
                PC_INC <= '1';
                NEXSTATE <= state_start;
                -----------------------

                -- Instruction > 0x3E --
            WHEN state_move_address_right =>
                PTR_INC <= '1';
                PC_INC <= '1';
                NEXSTATE <= state_start;
                -----------------------

                -- Instruction + 0x2B --
            WHEN state_ram_data_inc_read =>
                DATA_EN <= '1';
                DATA_WE <= '0';
                NEXSTATE <= state_ram_data_inc_set;

            WHEN state_ram_data_inc_set =>
                MX_SLC <= "01";
                NEXSTATE <= state_ram_data_inc_write;

            WHEN state_ram_data_inc_write =>
                DATA_EN <= '1';
                DATA_WE <= '1';
                PC_INC <= '1';
                NEXSTATE <= state_start;
                -----------------------

                -- Instruction - 0x2D --
            WHEN state_ram_data_dec_read =>
                DATA_EN <= '1';
                DATA_WE <= '0';
                NEXSTATE <= state_ram_data_dec_set;

            WHEN state_ram_data_dec_set =>
                MX_SLC <= "10";
                NEXSTATE <= state_ram_data_dec_write;

            WHEN state_ram_data_dec_write =>
                DATA_EN <= '1';
                DATA_WE <= '1';
                PC_INC <= '1';
                NEXSTATE <= state_start;
                ------------------------

                -- Instruction [ 0x5B --
            WHEN state_loop_start_init =>
                DATA_EN <= '1';
                DATA_WE <= '0';

                PC_INC <= '1';
                NEXSTATE <= state_loop_start_0;

            WHEN state_loop_start_0 =>
                IF (DATA_RDATA = (DATA_RDATA'RANGE => '0')) THEN
                    CNT_INC <= '1';
                    CODE_EN <= '1';
                    NEXSTATE <= state_loop_start_1;
                ELSE
                    RAS_PUSH <= '1';
                    NEXSTATE <= state_start;
                END IF;

            WHEN state_loop_start_1 =>
                IF (CNT = (CNT'RANGE => '0')) THEN
                    NEXSTATE <= state_start;
                ELSE
                    IF CODE_DATA = x"5b" THEN
                        CNT_INC <= '1';
                    ELSIF CODE_DATA = x"5d" THEN
                        CNT_DEC <= '1';
                    END IF;
                    PC_INC <= '1';
                    NEXSTATE <= state_loop_skip_instruction;
                END IF;

            WHEN state_loop_skip_instruction =>
                CODE_EN <= '1';
                NEXSTATE <= state_loop_start_1;

                ------------------------

                -- Instruction ] 0x5D --
            WHEN state_loop_end_init =>
                DATA_EN <= '1';
                DATA_WE <= '0';
                NEXSTATE <= state_loop_end_0;

            WHEN state_loop_end_0 =>
                IF (DATA_RDATA = (DATA_RDATA'RANGE => '0')) THEN
                    PC_INC <= '1';
                    RAS_POP <= '1';
                    NEXSTATE <= state_start;
                ELSE
                    RAS_TOP <= '1';
                    NEXSTATE <= state_start;
                END IF;
                ------------------------

                -- Instruction . 0x2E --
            WHEN state_write_init =>
                DATA_EN <= '1';
                DATA_WE <= '0';
                NEXSTATE <= state_write_set;

            WHEN state_write_set =>
                IF (OUT_BUSY = '1') THEN
                    DATA_EN <= '1';
                    DATA_WE <= '0';
                    NEXSTATE <= state_write_set;
                ELSE
                    OUT_WE <= '1';
                    PC_INC <= '1';
                    NEXSTATE <= state_start;
                END IF;
                ------------------------

                -- Instruction , 0x2C --
            WHEN state_read_init =>
                IN_REQ <= '1';
                MX_SLC <= "00";
                NEXSTATE <= state_read_set;

            WHEN state_read_set =>
                IF (IN_VLD = '0') THEN
                    IN_REQ <= '1';
                    MX_SLC <= "00";
                    NEXSTATE <= state_read_set;
                ELSE
                    DATA_EN <= '1';
                    DATA_WE <= '1';
                    PC_INC <= '1';
                    NEXSTATE <= state_start;
                END IF;
                ------------------------

            WHEN state_others => --Skip others symbol
                PC_INC <= '1';
                NEXSTATE <= state_start;
                ------------------------

            WHEN state_null => --End of program
                NEXSTATE <= state_null;
                ------------------------

            WHEN OTHERS => -- Undefined actions
                NEXSTATE <= state_start;
                ------------------------
        END CASE;
    END PROCESS NEXT_STATE_LOGIC;

END behavioral;