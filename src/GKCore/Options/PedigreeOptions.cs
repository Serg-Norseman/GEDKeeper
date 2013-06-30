using System;
using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
    [Serializable]
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

		private TPedigreeFormat FFormat;
		private bool FIncludeNotes;
		private bool FIncludeAttributes;
		private bool FIncludeSources;

		public TPedigreeFormat Format
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

		public void LoadFromFile(IniFile iniFile)
		{
            try
            {
                this.FIncludeAttributes = iniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                this.FIncludeNotes = iniFile.ReadBool("Pedigree", "IncludeNotes", true);
                this.FIncludeSources = iniFile.ReadBool("Pedigree", "IncludeSources", true);
                this.FFormat = (PedigreeOptions.TPedigreeFormat)iniFile.ReadInteger("Pedigree", "Format", 0);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading PedigreeOptions"); // FIXME
            }
		}

		public void SaveToFile(IniFile iniFile)
		{
			iniFile.WriteBool("Pedigree", "IncludeAttributes", this.FIncludeAttributes);
			iniFile.WriteBool("Pedigree", "IncludeNotes", this.FIncludeNotes);
			iniFile.WriteBool("Pedigree", "IncludeSources", this.FIncludeSources);
			iniFile.WriteInteger("Pedigree", "Format", (int)((sbyte)this.FFormat));
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
