﻿using System;
using System.IO;
using System.Text;

namespace CobolTranslateGen
{
  class CobolTranslateGen
  {
    static Encoding encodingIn;
    static Encoding encodingOut;
    static string fileOut;
    static StreamWriter swOut;

    static int Main(string[] args)
    {
      int retCode;

      try
      {
        retCode = GatherInputs(args);

        if (retCode == 0)
        {
          using (swOut = new StreamWriter(fileOut))
          {
            swOut.WriteLine("      *================================================================*");
            swOut.WriteLine("      *        Table generated by C# program CobolTranslateGen         *");
            swOut.WriteLine("      *================================================================*");
            swOut.WriteLine();
            swOut.WriteLine($"       01  W-{args[0]}-TO-{args[1]}-TABLE.");

            for (int i = 0x00; i <= 0xF0; i += 16)
            {
              byte byteIn = (byte) i;

              swOut.WriteLine($"           05  FILLER-{byteIn.ToString("X2")}-{(byteIn + 15).ToString("X2")}        PIC X(32)       VALUE");

              byte[] encInBytes = new byte[16];

              for (int j = 0; j < 16; j++)
              {
                encInBytes[j] = (byte) (i + j);    
              }

              byte[] encOutBytes = Encoding.Convert(encodingIn, encodingOut, encInBytes);

              swOut.Write("               '");

              for (int j = 0; j < 16; j++)
              {
                swOut.Write(encOutBytes[j].ToString("X2"));
              }

              swOut.WriteLine("'.");
            }
          }
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
        retCode = 9;
      }

      return retCode;
    }

    private static int GatherInputs(string[] args)
    {
      Console.WriteLine("Available encodings:");
      Console.Write("    CodePage identifier and name     ");
      Console.Write("BrDisp   BrSave   ");
      Console.Write("MNDisp   MNSave   ");
      Console.WriteLine("1-Byte   ReadOnly ");

      // For every encoding, get the property values.
      foreach (EncodingInfo ei in Encoding.GetEncodings())
      {
        Encoding e = ei.GetEncoding();

        Console.Write("    {0,-6} {1,-25} ", ei.CodePage, ei.Name);
        Console.Write("{0,-8} {1,-8} ", e.IsBrowserDisplay, e.IsBrowserSave);
        Console.Write("{0,-8} {1,-8} ", e.IsMailNewsDisplay, e.IsMailNewsSave);
        Console.WriteLine("{0,-8} {1,-8} ", e.IsSingleByte, e.IsReadOnly);
      }

      if (args.Length != 3)
      {
        Console.WriteLine($"Usage: encodingFrom encodingTo fileOut");
        return 1;
      }

      Console.WriteLine($"Encoding in:  {args[0]}");
      Console.WriteLine($"Encoding out: {args[1]}");
      Console.WriteLine($"Output file:  {args[2]}");

      encodingIn = Encoding.GetEncoding(args[0]);
      encodingOut = Encoding.GetEncoding(args[1]);
      fileOut = args[2];

      return 0;
    }
  }
}