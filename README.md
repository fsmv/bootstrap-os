# bootstrap-os

A bootable x86 code editor that lets you run the code you type in directly on
real hardware. Enough to bootstrap anything with no other tools.

![bootstrap-hex demo](images/v0.4-hex-2x.gif)

Above is v0.4 (bootstrap-hex.asm), the latest release. Technically it is all you
need to create anything you want. For the full bootstrapping experience you'd
have to write your assembly on paper and hand-assemble it before typing it in
carefully.

Since you'd probably write a text editor and an assembler first thing, I think
it makes sense to just start there. I'm
[currently working on](https://github.com/fsmv/bootstrap-os/tree/assembler)
exactly that.

## Compatibility

This code should run correctly on any x86 PC compatible machine made between
1984 and 2020. I believe the earilest machine that will run it is the IBM PC AT
with the EGA card expansion released in October 1984.

If it doesn't boot on your machine that supports BIOS, please let me know. I'd
love to find out why.

### Compatibility details

This code is 16 bit real mode x86 assembly which Intel has kept available on all
x86 CPUs since the original 8086 processor from 1976. The assembly code itself
is compatible with the 8086.

It also depends on the IBM PC BIOS hardware interface standard started in 1981
which modern computers still implement. Sadly Intel has partially ended this
incredible nealy 40 year backwards compatibility story by officially
[ending support for BIOS](https://www.bleepingcomputer.com/news/hardware/intel-plans-to-end-legacy-bios-support-by-2020/)
as of 2020, so now there are some machines on the market that only support UEFI
booting.

The latest BIOS call used here is int 0x10 with ah = 0x13 for printing a string.
This was introduced with PC AT although the earlier XT machines got this feature
later with a BIOS update. Additionally it uses BIOS functions to set the
overscan color and blink bit behavior which where added with the EGA card, so it
is technically not compatible with the CGA card although it will mostly work.

If we had a replacement for BIOS print string based on the print character
interrupts, technically it could be compatible with the original PC from 1981.
However the lisp interpreter code is more than the 8 sectors per track the PC
supported and the bootsector code expects to have multi-track read support which
the PC didn't have originially (maybe you could get a BIOS that does have it).

Additionally the text editor requires 256k of RAM which was the maximum possible
PC configuration.

## Why?

Mainly I'm jealous of the people who grew up with computers that booted to a
basic editor and had computer magazines with code to type in.

I hope people will use this to learn more about the computers we use at a low
level and have fun running real assembly directly on the CPU with no other code
involved. Also I hope that some college classes might try teaching assembly with
real x86 using this instead of using a mips simulator (because I for one have
never seen a mips CPU).

I think modern software has gotten too far from the hardware. Also I thought it
would be fun to escape
[The 30 Million Line Problem](https://www.youtube.com/watch?v=kZRE7HIO3vk)
for a while.

## Booting it

### On Real Hardware

1. Get the binary from the releases or use the boot script (assemble it with
   `nasm`)
2. Write it to a usb drive: `dd if=bin/bootstrap-hex.bin of=/dev/sdb && sync`
   - Warning: make sure to change the output to the right path for your system.
     Use `fdisk -l` to list them.
   - You can backup the first couple sectors if you don't want to reformat the
     drive later with: `dd if=/dev/sdb of=sdb-10-sectors.bak bs=512 count=10`
3. Keep the USB plugged in and boot it on your computer in BIOS mode by pressing
   F2 or F11 during startup to get the system menu (also USB booting sometimes
   needs to be enabled in the setup menu)

### In QEMU

#### Linux / MacOS

1. Install `qemu` and `nasm` with your package manager of choice.
2. `./boot bootstrap-lisp.asm`

#### Debugging on Linux / MacOS

You can also run `./debug bootstrap-lisp.asm` to attach gdb to qemu with a
special config for 16 bit real mode and it has symbols properly loaded and
everything.

In `gdbconf/gdbinit_real_mode.txt` several new gdb commands are defined. The
most important is `stepo` which steps over a call instruction skipping the body of
the call, but you can open up that file and try out the other commands too.

#### Windows

 1. Install qemu: https://www.qemu.org/download/#windows
 2. Install nasm: https://www.nasm.us/pub/nasm/snapshots/latest/win64/
 3. Set the PATH variable:
    1. In the start menu, search for and open the environment variables editor 
    2. Choose either the user or system-wide Path variable and add both `C:\Program Files\NASM` & `C:\Program Files\qemu`
 4. `boot.bat bootstrap-lisp.asm` (alternatively you can use the bash script in the git bash prompt)

#### Android

 1. Install [Termux](https://play.google.com/store/apps/details/id=com.termux)
    from the play store (or f-droid)

 2. Install qemu and the assembler, and git to clone the repo
    `pkg install x11-repo && pkg install qemu-system-x86-64 nasm git`

 3. (Optional) Set up X11 for graphical display

    1. Install the Termux X11 apk from
       https://github.com/termux/termux-x11/releases (you most likely need the
       arm64 one), or from f-droid
    2. In the Termux X11 app settings configure the output display resolution
       mode to custom and the resolution to 720x480
    3. Install the necessary packages in the main Termux app:
       `pkg install termux-x11-nightly`

 4. Clone bootstrap-os `git clone https://github.com/fsmv/bootstrap-os.git && cd bootstrap-os`

 5. To run in Termux:
    1. `export QEMU_ARGS="-display gtk,show-menubar=off -L /data/data/com.termux/files/usr/share/qemu"`
       If you like you can put this in `~/.bashrc` to save it
    2. `termux-x11 :0 -xstartup "./boot bootstrap-lisp.asm"`
       - Alternatively (if you didn't install the X11 app) you can run:
         `./test -DHEADLESS lisp/tester.asm`
         it will print the test results using the qemu debug console feature.

I tried to run the debugger but I wasn't able to install a version of gdb that
understands x86 in Termux (it only does arm) so debugging won't work.

## Reference Manuals

This project depends on the
[Intel Architecture Manual](https://software.intel.com/sites/default/files/managed/39/c5/325462-sdm-vol-1-2abcd-3abcd.pdf)
and the [1988 IBM BIOS Manual](http://bitsavers.trailing-edge.com/pdf/ibm/pc/ps2/15F0306_PS2_and_PC_BIOS_Interface_Technical_Reference_May88.pdf).
There's also the [1992 IBM VGA Card Manual](http://bitsavers.trailing-edge.com/pdf/ibm/pc/cards/IBM_VGA_XGA_Technical_Reference_Manual_May92.pdf)
which isn't really used in this project so far but would be useful for anyone
who wants to do graphics programming after BIOS booting.

Modern x86 computers still implement these standards and it makes me happy to
use the original documentation. Unfortunately to actually boot on modern
computers you have to comply with some manufacturer expectations added over the
years which I learned about on https://wiki.osdev.org/

### Script Instructions

`reference_manuals/get_docs` is script Bash users can run to download these
manuals and also optionally adds bookmarks to the IBM BIOS manual (which I
manually typed out) using `pdflatex` (you'll need to install that to add the
bookmarks).

Windows users could run it in WSL or copy the pdflatex commands out of the
script.

## Make Your Own Bootable Code

You can simply `%include` the `util/bootsect-header.asm` at the top of your
bootsector code and `util/bootsect-footer.asm` at the bottom. Feel free to
customize it and check out the comments for details. These two files contain all
the details to allow you to BIOS boot on real hardware.

Once you go over the single sector size nasm will give you an error. At that
point you'll have to put any additional code after `util/bootsect-footer.asm`
and write some code inside the bootsector to load any additional sectors from
disk (since the BIOS won't do it for you). Check out the bootsector code I'm
using for an example.
