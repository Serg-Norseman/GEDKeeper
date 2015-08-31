using System;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
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
