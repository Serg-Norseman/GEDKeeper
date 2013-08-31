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

		public TPedigreeFormat Format
		{
			get;
			set;
		}

		public bool IncludeAttributes
		{
			get;
			set;
		}

		public bool IncludeNotes
		{
			get;
			set;
		}

		public bool IncludeSources
		{
			get;
			set;
		}

		public PedigreeOptions()
		{
			this.IncludeAttributes = true;
			this.IncludeNotes = true;
			this.IncludeSources = true;
		}

		public void LoadFromFile(IniFile iniFile)
		{
            try
            {
                this.IncludeAttributes = iniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                this.IncludeNotes = iniFile.ReadBool("Pedigree", "IncludeNotes", true);
                this.IncludeSources = iniFile.ReadBool("Pedigree", "IncludeSources", true);
                this.Format = (PedigreeOptions.TPedigreeFormat)iniFile.ReadInteger("Pedigree", "Format", 0);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading PedigreeOptions"); // FIXME
            }
		}

		public void SaveToFile(IniFile iniFile)
		{
			iniFile.WriteBool("Pedigree", "IncludeAttributes", this.IncludeAttributes);
			iniFile.WriteBool("Pedigree", "IncludeNotes", this.IncludeNotes);
			iniFile.WriteBool("Pedigree", "IncludeSources", this.IncludeSources);
			iniFile.WriteInteger("Pedigree", "Format", (int)((sbyte)this.Format));
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
