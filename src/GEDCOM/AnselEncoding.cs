/*
 * $Id: AnselEncoding.cs 199 2008-11-15 15:20:44Z davek $
 * 
 * AnselEncoding.cs - Based of implementation of the "System.Text.ASCIIEncoding" class in mono
 * As such this file is NOT GPL, but expat is GPL compatible so we are ok using it
 * see http://www.fsf.org/licensing/licenses/
 *
 * Copyright (c) 2001  Southern Storm Software, Pty Ltd
 * Copyright (C) 2003 Novell, Inc.
 * Copyright (C) 2004 Novell, Inc (http://www.novell.com)
 * 
 * Copyright (C) 2008 David A Knight <david@ritter.demon.co.uk>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

using System;
using System.Text;

namespace GedCom551
{
	public sealed class AnselEncoding : Encoding
	{
		// Magic number used by Windows for "ANSEL" is ?
		internal const int ANSEL_CODE_PAGE = 20127; // this is ASCII not ANSEL
		
		static readonly int[] marc8 = new int[]
		{
			0x88, 0x89, 0x8D, 0x8E, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,
			0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xB0, 0xB1,
			0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBC,
			0xBD, 0xBE /* LDS Extension empty box */, 0xBF /* LDS Extension black box */, 0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6,
			0xC7, 0xC8, 0xCD /* LDS Extension midline e */, 0xCE /* LDS Extension midline o */, 0xCF /* LDS Extension es zet */,
			// combiners
			0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9,
			0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, 0xF0, 0xF1, 0xF2, 0xF3,
			0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFE
		};
		const int marc8CombinerStart = 45;
		static readonly int[] ucs = new int[]
		{
			0x0098, 0X009C, 0x200D, 0x200C, 0x0141, 0x00D8, 0x0110, 0x00DE, 0x00C6, 0x0152,
			0x02B9, 0x00B7, 0x266D, 0x00AE, 0x00B1, 0x01A0, 0x01AF, 0x02BC, 0x02BB, 0x0142,
			0x00F8, 0x0111, 0x00FE, 0x00e6, 0x0153, 0x02BA, 0x0131, 0x00A3, 0x00F0, 0x01A1,
			0x01B0, 0x25AB /* LDS Extension empty box */, 0x25aa /* LDS Extension black box */, 0x00B0, 0x2113, 0x2117, 0x00A9, 0x266F, 0x00BF, 0x00A1,
			0x00DF, 0x20AC,	(int)'?', (int)'?', 0x00DF/* LFS Extension es zet */, 
			// combiners
			0x0309, 0x0300, 0x0301, 0x0302, 0x0303, 0x0304, 0x0306, 0x0307, 0x0308, 0x030C,
			0x030A,	0x0361/*0xfe20*/, 0xFE21, 0x0315, 0x030B, 0x0310, 0x0327, 0x0328, 0x0323, 0x0324, 
			0x0325, 0x0333, 0x0332, 0x0326, 0x031C, 0x032E, 0x0360 /*0xfe22*/, 0xfe23, 0x0313	
		};
				
		public AnselEncoding() : base(ANSEL_CODE_PAGE)
		{
		}

		public override string BodyName
		{
			get { return "ansel"; }
		}
		public override string HeaderName
		{
			get { return "ansel"; }	
		}
		public override string WebName
		{
			get { return "ansel"; }
		}
		public override string EncodingName
		{
			get { return "ANSEL"; }
		}
		public override bool IsMailNewsDisplay
		{
			get { return false; }
		}
		public override bool IsMailNewsSave
		{
			get { return false; }
		}
		
		private static int GetMarc8Index(int c)
		{
			int i = 0;
			foreach (int marcChar in marc8) 
			{
				if (marcChar == c)
				{
					break;
				}
				i ++;
			}
			if (i == marc8.Length)
			{
				i = -1;
			}
			
			return i;
		}
		
		private static int GetUCSIndex(int c)
		{
			int i = 0;
			foreach (int ucsChar in ucs) 
			{
				if (ucsChar == c)
				{
					break;
				}
				i ++;
			}
			if (i == ucs.Length)
			{
				i = -1;
			}
			
			return i;
		}
		
		public override bool IsSingleByte
		{
			get { return true; }
		}
		
			
		public override int GetByteCount(char[] chars, int index, int count)
		{
			if (chars == null)
			{
				throw new ArgumentNullException("chars");
			}
			if (index < 0 || index > chars.Length)
			{
				throw new ArgumentOutOfRangeException("index", "ArgRange_Array");	
			}
			if (count < 0 || count > (chars.Length - index))
			{
				throw new ArgumentOutOfRangeException("count", "ArgRange_Array");
			}
			
			int c = 0;
			
			while (count > 0)
			{
				if ((int)chars[index + c] <= 0x7f)
				{
					c ++;
				}
				else
				{
					int i = GetUCSIndex((int)chars[index + c]);
					c ++;
					if (i >= marc8CombinerStart)
					{
						c ++;
					}
				}
				count --;
			}
						
			return c;
		}
		
		public override int GetByteCount(string s)
		{
			if (s == null)
			{
				throw new ArgumentNullException("s");
			}
			
			int count = 0;
			
			for (int c = 0; c < s.Length; c ++)
			{
				if ((int)s[c] <= 0x7f)
				{
					count ++;
				}
				else
				{
					int i = GetUCSIndex((int)s[c]);
					count ++;
					if (i >= marc8CombinerStart)
					{
						count ++;
					}
				}
			}
			
			return count;
		}

		public override int GetBytes(char[] chars, int charIndex, int charCount, byte[] bytes, int byteIndex)
		{
			if (chars == null)
			{
				throw new ArgumentNullException("chars");
			}
			if (bytes == null)
			{
				throw new ArgumentNullException("bytes");
			}
			if (charIndex < 0 || charIndex > chars.Length)
			{
				throw new ArgumentOutOfRangeException("charIndex", "ArgRange_Array");
			}
			if (byteIndex < 0 || byteIndex > bytes.Length)
			{
				throw new ArgumentOutOfRangeException("byteIndex", "ArgRange_Array");
			}
			if ((bytes.Length - byteIndex) < charCount)
			{
				throw new ArgumentException("Arg_InsufficientSpace");
			}
			int count = charCount;
			char ch;
			while (count -- > 0)
			{
				ch = chars[charIndex++];
				if (ch < (char)0x80)
				{
					bytes[byteIndex ++] = (byte)ch;
				}
				else
				{
					int i = GetUCSIndex(ch);
					if (i != -1)
					{
						bytes[byteIndex ++] = (byte)marc8[i];
					}
					else
					{
						bytes[byteIndex ++] = (byte)'?';
					}
				}
			}
			
			return charCount;
		}
		
		public override int GetBytes(string s, int charIndex, int charCount, byte[] bytes, int byteIndex)
		{
			if (s == null)
			{
				throw new ArgumentNullException("s");
			}
			if (bytes == null)
			{
				throw new ArgumentNullException("bytes");
			}
			if (charIndex < 0 || charIndex > s.Length)
			{
				throw new ArgumentOutOfRangeException("charIndex", "ArgRange_Array");
			}
			if (byteIndex < 0 || byteIndex > bytes.Length)
			{
				throw new ArgumentOutOfRangeException("byteIndex", "ArgRange_Array");
			}
			if ((bytes.Length - byteIndex) < charCount)
			{
				throw new ArgumentException("Arg_InsufficientSpace");
			}
			int count = charCount;
			char ch;
			while (count -- > 0)
			{
				ch = s[charIndex++];
				if (ch < (char)0x80)
				{
					bytes[byteIndex ++] = (byte)ch;
				}
				else
				{
					int i = GetUCSIndex(ch);
					if (i != -1)
					{
						// need to swap combiner position
						if (i >= marc8CombinerStart)
						{
							byte b = bytes[byteIndex - 1];
							bytes[byteIndex - 1] = (byte)marc8[i];
							bytes[byteIndex ++] = b;
						}
						else
						{
							bytes[byteIndex ++] = (byte)marc8[i];
						}
					}
					else
					{
						bytes[byteIndex ++] = (byte)'?';
					}
				}
			}
			
			return charCount;
		}

		
		public override int GetCharCount(byte[] bytes, int index, int count)
		{
			if (bytes == null) 
			{
				throw new ArgumentNullException("bytes");
			}
			if (index < 0 || index > bytes.Length) 
			{
				throw new ArgumentOutOfRangeException("index", "ArgRange_Array");
			}
			if (count < 0 || count > (bytes.Length - index)) 
			{
				throw new ArgumentOutOfRangeException ("count", "ArgRange_Array");
			}
			
			int c = 0;
			while (count > 0)
			{
				byte b = bytes[index + c];
				
				c++;
				
				if (b > 0x7f)
				{ 
					int i = GetMarc8Index((int)b);
					
					if (i >= marc8CombinerStart)
					{
						c ++;
						count --;
					}
				}
				count --;
			}
			// possible if we have a combiner byte but nothing to combine to
			if (count < 0)
			{
				c --;
			}
			
			return c;
		}

	
		
		public override int GetChars(byte[] bytes, int byteIndex, int byteCount, char[] chars, int charIndex)
		{
			if (bytes == null)
			{
				throw new ArgumentNullException("bytes");
			}
			if (chars == null) 
			{
				throw new ArgumentNullException("chars");
			}
			if (byteIndex < 0 || byteIndex > bytes.Length) 
			{
				throw new ArgumentOutOfRangeException("byteIndex", "ArgRange_Array");
			}
			if (byteCount < 0 || byteCount > (bytes.Length - byteIndex)) 
			{
				throw new ArgumentOutOfRangeException ("byteCount", "ArgRange_Array");
			}
			if (charIndex < 0 || charIndex > chars.Length) 
			{
				throw new ArgumentOutOfRangeException ("charIndex", "ArgRange_Array");
			}
			if ((chars.Length - charIndex) < byteCount) 
			{
				throw new ArgumentException ("Arg_InsufficientSpace");
			}
			
			int count = byteCount;
			bool combine = false;
			while (count-- > 0) 
			{
				char c = (char) bytes[byteIndex++];
				if (c < '\x80')
				{
					chars[charIndex++] = c;
					if (combine)
					{
						charIndex ++;
						combine = false;
					}
				}
				else 
				{
					int i = GetMarc8Index(c);
					if (i != -1)
					{
						if (i < marc8CombinerStart)
						{
							chars[charIndex++] = (char)ucs[i];
						}
						else
						{
							// should never happen
							if (combine)
							{
								charIndex ++;
								combine = false;
							}
							chars[charIndex + 1] = (char)ucs[i];
							combine = true;
						}
					}
					else
					{
						chars[charIndex ++] = '?';
					}
				}
			}
		
			return byteCount;
		}

		public override int GetMaxCharCount(int byteCount)
		{
			if (byteCount < 0) 
			{
				throw new ArgumentOutOfRangeException ("byteCount", "ArgRange_NonNegative");
			}
			
			return byteCount;
		}

		
		public override int GetMaxByteCount(int charCount)
		{
			if (charCount < 0) 
			{
				throw new ArgumentOutOfRangeException ("charCount", "ArgRange_NonNegative");
			}
			
			return (charCount * 2);
		}

		public override string GetString(byte[] bytes, int index, int count)
		{
			if (bytes == null)
			{
				throw new ArgumentNullException("bytes");
			}
			if (index < 0 || index > bytes.Length)
			{
				throw new ArgumentOutOfRangeException("index", "ArgRange_Array");
			}
			if (count < 0 || count > (bytes.Length - index))
			{
				throw new ArgumentOutOfRangeException("count", "ArgRange_Array");
			}
			if (count == 0)
			{
				return string.Empty;
			}
			// probably horribly inefficient but...
			char[] chars = new char[count];
			GetChars(bytes, index, count, chars, 0);
			string s = new string(chars);
			
			return s;
		}
		
	}
}
