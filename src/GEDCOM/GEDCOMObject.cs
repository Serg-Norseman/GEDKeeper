using System;

/// <summary>
/// 
/// </summary>

namespace GedCom551
{
    [Serializable]
	public class EGEDCOMException : Exception
	{
		public EGEDCOMException()
		{
		}

		public EGEDCOMException(string message) : base(message)
		{
		}
	}

    public class GEDCOMObject : IDisposable
	{
		public const char GEDCOMDelimiter = ' ';
		public const char GEDCOMYearModifierSeparator = '/';
		public const string GEDCOMYearBC = "B.C."; // const restored
		public const char GEDCOMPointerDelimiter = '@';

		// deprecated
		//public const string GEDCOMNewLine = "#13#10";
		//public const byte GEDCOMMaxPhoneNumbers = 3;
		//public const byte GEDCOMMaxEmailAddresses = 3;
		//public const byte GEDCOMMaxFaxNumbers = 3;
		//public const byte GEDCOMMaxWebPages = 3;
		//public const byte GEDCOMMaxLanguages = 3;

		//public static readonly string[] BloodGroups = new string[] { "(I) O+", "(I) O-", "(II) A+", "(II) A-", "(III) B+", "(III) B-", "(IV) AB+", "(IV) AB-" };

        private bool fDisposed;

		public object ExtData
		{
			get;
			set;
		}

        protected virtual void Dispose(bool disposing)
        {
        }

        public void Dispose()
        {
            if (!this.fDisposed)
            {
                this.Dispose(true /*called by user directly*/);
                this.fDisposed = true;
            }

            GC.SuppressFinalize(this);
        }

        ~GEDCOMObject()
        {
            Dispose(false /*not called by user directly*/);
        }
    }
}
