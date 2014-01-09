using System;
using System.Text;

using Ext.Utils;

/// <summary>
/// Localization: clean
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

	public class TGEDCOMObject
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

		public object ExtData
		{
			get;
			set;
		}

		public void Free()
		{
			SysUtils.Free(this);
		}

	}
}
