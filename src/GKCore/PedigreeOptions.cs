using System;
using System.Runtime.InteropServices;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
    public class EPedigreeOptionsException : Exception
    {
        public EPedigreeOptionsException()
        {
        }
        public EPedigreeOptionsException(string message) : base(message)
        {
        }
    }

    public class PedigreeOptions
	{
		public enum TPedigreeFormat : byte { pfExcess, pfCompact }

		private PedigreeOptions.TPedigreeFormat FFormat;
		private bool FIncludeNotes;
		private bool FIncludeAttributes;
		private bool FIncludeSources;

		public PedigreeOptions.TPedigreeFormat Format
		{
			get { return this.FFormat; }
			set { this.FFormat = value; }
		}

		public bool IncludeAttributes
		{
			get { return this.FIncludeAttributes; }
			set { this.FIncludeAttributes = value; }
		}

		public bool IncludeNotes
		{
			get { return this.FIncludeNotes; }
			set { this.FIncludeNotes = value; }
		}

		public bool IncludeSources
		{
			get { return this.FIncludeSources; }
			set { this.FIncludeSources = value; }
		}

		public PedigreeOptions()
		{
			this.FIncludeAttributes = true;
			this.FIncludeNotes = true;
			this.FIncludeSources = true;
		}

		public void LoadFromFile([In] IniFile aIniFile)
		{
            try
            {
                this.FIncludeAttributes = aIniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                this.FIncludeNotes = aIniFile.ReadBool("Pedigree", "IncludeNotes", true);
                this.FIncludeSources = aIniFile.ReadBool("Pedigree", "IncludeSources", true);
                this.FFormat = (PedigreeOptions.TPedigreeFormat)aIniFile.ReadInteger("Pedigree", "Format", 0);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading PedigreeOptions"); // FIXME
            }
		}

		public void SaveToFile([In] IniFile aIniFile)
		{
			aIniFile.WriteBool("Pedigree", "IncludeAttributes", this.FIncludeAttributes);
			aIniFile.WriteBool("Pedigree", "IncludeNotes", this.FIncludeNotes);
			aIniFile.WriteBool("Pedigree", "IncludeSources", this.FIncludeSources);
			aIniFile.WriteInteger("Pedigree", "Format", (int)((sbyte)this.FFormat));
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
