# GC-Examples

Various example GnuCOBOL programs packaged for easy compiling and running using Visual Studio Code.

## Getting Started

These instructions will get you a copy of the project up and running on your local Windows machine for development and testing purposes.

### Prerequisites

1. GnuCOBOL
2. Your favorite IDE (mine is the free Visual Studio Code)

### Installing

1. Download GnuCOBOL:
    See the options at https://www.arnoldtrembley.com/GnuCOBOL.htm.
    For Windows 64 bit, a real easy way to go is https://www.arnoldtrembley.com/GC30B-64bit-rename-7z-to-exe.7z, and the following instructions assume this.

2. Rename the '.7z' to '.exe' and run it, or use 7-Zip to extract all folders/files to C:\GnuCOBOL (or your desired location). A Windows install is not required. Your folder structure should look like this:

<pre>
    GnuCOBOL   
        bin  
        etc  
        include  
        lib  
        share  
        x86_64-w64-mingw32 
</pre>

3. If you do not already have VS Code, download and install it from here: https://code.visualstudio.com/download.

4. Download the GC-Examples project, e.g. to C:\GC-Examples. Your folder structure should look something like this:

<pre>
    GC-Examples  
        .vscode  
        CopyBooks  
        Data
        Listings  
</pre>

5. Run VS Code and select File/Open Folder..., and open the above GC-Examples folder to get started. It is important to do the Open Folder instead of opening individual files!

## Running the example programs

1. **RDWADJST**: This program adjusts the Record Descriptor Word (RDW) on variable length record files, to support uploading and downloading such files to/from an IBM mainframe. GnuCOBOL expects RDWs to have a record length excluding the RDW while the mainframe RDWs have a record length including the RDW, i.e. the GnuCOBOL RDW record length is 4 less than the mainframe.

    To compile and run it from VS Code, select Terminal/Run Task.../Compile and Run RDWADJST.

    The program supports GnuCOBOL files read/written with COB_VARSEQ_FORMAT = 0, i.e. RDWs have a 2 byte record length (big-endian) + 2 NULs.

    The program must be compiled with option std=mf which supports COMP-X fields and CBL_OPEN_FILE, etc.

    The program requires 2 run-time arguments:
    1. **File name** containing RDWs to be updated (RDWs are updated in place).
    2. **GC2MF** or **MF2GC** to increase or decrease RDW record lengths by 4:<br>
        **GC2MF** is GnuCOBOL to Mainframe,<br>
        **MF2GC** is Mainframe to GnuCOBOL.

    When uploading/downloading mainframe files, FTP in binary mode and specify "quote site rdw" to have the mainframe include the RDWs. 

    HxD is a really useful tool for viewing files in hex (e.g. to examine RDWs), and it supports many character sets, including EBCDIC and ASCII. See https://mh-nexus.de/en/hxd/. 

2. **TESTIO1**: Test reading a line sequential file. This shows that GnuCOBOL treats CR/LF (on Windows) as end of record markers, whereas those are ignored on regular sequential files. Try removing the LINE from "ORGANIZATION LINE SEQUENTIAL" to see the difference with the provided input file.

    To compile and run it from VS Code, click on TESTIO1.cob and select Terminal/Run Task.../Compile and Run Selected Program.

3. **TESTIO2**: Test writing a variable length record sequential file. This shows that GnuCOBOL creates RDWs similar to the IBM mainframe (when COB_VARSEQ_FORMAT=0, the default), but the RDW record length does not include itself, i.e. it is 4 less than the mainframe.

    To compile and run it from VS Code, click on TESTIO2.cob and select Terminal/Run Task.../Compile and Run Selected Program.

4. **TRNSLATT**: Test converting a file from ASCII to EBCDIC, using subroutine **TRNSLAT**.

    To compile and run it from VS Code, select Terminal/Run Task.../Compile and Run TRNSLATT.

## Authors

* **Brian D Pead** - *Initial work*

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

Many thanks to the following:

* Arnold Trembley for the GnuCOBOL builds and instructions - see https://www.arnoldtrembley.com/.
* The many developers of GnuCOBOL and its predecessors - see https://sourceforge.net/projects/open-cobol/.
* The developers of HxD - see https://mh-nexus.de/en/hxd/.
