using System;
using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

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

    public sealed class PedigreeOptions : IDisposable
	{
		public enum PedigreeFormat { pfExcess, pfCompact }

        private bool fDisposed;

		public PedigreeFormat Format
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

        public void Dispose()
        {
            if (!this.fDisposed)
            {
                this.fDisposed = true;
            }
        }

		public void LoadFromFile(IniFile iniFile)
		{
            if (iniFile == null) return;

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
            if (iniFile == null) return;

			iniFile.WriteBool("Pedigree", "IncludeAttributes", this.IncludeAttributes);
			iniFile.WriteBool("Pedigree", "IncludeNotes", this.IncludeNotes);
			iniFile.WriteBool("Pedigree", "IncludeSources", this.IncludeSources);
			iniFile.WriteInteger("Pedigree", "Format", (sbyte)this.Format);
		}

    }
}
