using System;

using BSLib;
using GKCommon;
using GKCore.Types;

namespace GKCore.Options
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

    /// <summary>
    /// 
    /// </summary>
    public sealed class PedigreeOptions : BaseObject
    {
        public PedigreeFormat Format;
        public bool IncludeAttributes;
        public bool IncludeNotes;
        public bool IncludeSources;

		public PedigreeOptions()
		{
			this.IncludeAttributes = true;
			this.IncludeNotes = true;
			this.IncludeSources = true;
		}

		public void LoadFromFile(IniFile iniFile)
		{
            if (iniFile == null) {
                throw new ArgumentNullException("iniFile");
            }

            try
            {
                this.IncludeAttributes = iniFile.ReadBool("Pedigree", "IncludeAttributes", true);
                this.IncludeNotes = iniFile.ReadBool("Pedigree", "IncludeNotes", true);
                this.IncludeSources = iniFile.ReadBool("Pedigree", "IncludeSources", true);
                this.Format = (PedigreeFormat)iniFile.ReadInteger("Pedigree", "Format", 0);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading PedigreeOptions"); // FIXME
            }
		}

		public void SaveToFile(IniFile iniFile)
		{
            if (iniFile == null) {
                throw new ArgumentNullException("iniFile");
            }

			iniFile.WriteBool("Pedigree", "IncludeAttributes", this.IncludeAttributes);
			iniFile.WriteBool("Pedigree", "IncludeNotes", this.IncludeNotes);
			iniFile.WriteBool("Pedigree", "IncludeSources", this.IncludeSources);
			iniFile.WriteInteger("Pedigree", "Format", (sbyte)this.Format);
		}

    }
}
