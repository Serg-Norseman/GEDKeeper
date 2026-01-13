/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GEDCOMCharacterSet
    {
        csASCII,
        csANSEL,
        csUNICODE,
        csUTF8
    }


    public sealed class GDMHeaderSource : GDMValueTag
    {
        public string Version { get; set; }
        public string ProductName { get; set; }


        public GDMHeaderSource()
        {
            SetName(GEDCOMTagType.SOUR);
        }

        public override void Clear()
        {
            base.Clear();

            Version = string.Empty;
            ProductName = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(Version) && string.IsNullOrEmpty(ProductName);
        }
    }


    public sealed class GDMHeaderGEDCOM : GDMTag
    {
        public string Version { get; set; }
        public string Form { get; set; }


        public GDMHeaderGEDCOM()
        {
            SetName(GEDCOMTagType.GEDC);
        }

        public override void Clear()
        {
            base.Clear();

            Version = string.Empty;
            Form = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(Version) && string.IsNullOrEmpty(Form);
        }
    }


    public sealed class GDMHeaderCharSet : GDMTag
    {
        private GEDCOMCharacterSet fValue;

        public GEDCOMCharacterSet Value
        {
            get { return fValue; }
            set { fValue = value; }
        }

        public string Version { get; set; }


        public GDMHeaderCharSet()
        {
            SetName(GEDCOMTagType.CHAR);
        }

        public override void Clear()
        {
            base.Clear();

            Version = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(Version);
        }

        protected override string GetStringValue()
        {
            return GEDCOMUtils.GetCharacterSetStr(fValue);
        }

        public override string ParseString(string strValue)
        {
            fValue = GEDCOMUtils.GetCharacterSetVal(strValue);
            return string.Empty;
        }
    }


    public sealed class GDMHeaderFile : GDMValueTag
    {
        private string fUID;


        public int Revision { get; set; }

        public string UID
        {
            get {
                if (string.IsNullOrEmpty(fUID)) {
                    fUID = GEDCOMUtils.CreateUID();
                }
                return fUID;
            }
            set { fUID = value; }
        }


        public GDMHeaderFile()
        {
            SetName(GEDCOMTagType.FILE);
        }

        public override void Clear()
        {
            base.Clear();

            Revision = 0;
            fUID = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (Revision == 0) && string.IsNullOrEmpty(fUID);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMHeader : GDMTag
    {
        private readonly GDMHeaderCharSet fCharacterSet;
        private string fCopyright;
        private readonly GDMHeaderFile fFile;
        private readonly GDMHeaderGEDCOM fGEDCOM;
        private GDMLanguageID fLanguage;
        private readonly GDMTextTag fNote;
        private readonly GDMPlace fPlace;
        private string fReceivingSystemName;
        private readonly GDMHeaderSource fSource;
        private readonly GDMPointer fSubmission;
        private readonly GDMPointer fSubmitter;
        private DateTime fTransmissionDateTime;


        public GDMHeaderCharSet CharacterSet
        {
            get { return fCharacterSet; }
        }

        public string Copyright
        {
            get { return fCopyright; }
            set { fCopyright = value; }
        }

        public GDMHeaderFile File
        {
            get { return fFile; }
        }

        public GDMHeaderGEDCOM GEDCOM
        {
            get { return fGEDCOM; }
        }

        public GDMLanguageID Language
        {
            get { return fLanguage; }
            set { fLanguage = value; }
        }

        public GDMTextTag Note
        {
            get { return fNote; }
        }

        public GDMPlace Place
        {
            get { return fPlace; }
        }

        public string ReceivingSystemName
        {
            get { return fReceivingSystemName; }
            set { fReceivingSystemName = value; }
        }

        public GDMHeaderSource Source
        {
            get { return fSource; }
        }

        public GDMPointer Submission
        {
            get { return fSubmission; }
        }

        public GDMPointer Submitter
        {
            get { return fSubmitter; }
        }

        public DateTime TransmissionDateTime
        {
            get { return fTransmissionDateTime; }
            set { fTransmissionDateTime = value; }
        }


        public GDMHeader()
        {
            SetName(GEDCOMTagType.HEAD);

            fCharacterSet = new GDMHeaderCharSet();
            fFile = new GDMHeaderFile();
            fGEDCOM = new GDMHeaderGEDCOM();
            fNote = new GDMTextTag((int)GEDCOMTagType.NOTE);
            fPlace = new GDMPlace();
            fSource = new GDMHeaderSource();
            fSubmission = new GDMPointer((int)GEDCOMTagType.SUBN);
            fSubmitter = new GDMPointer((int)GEDCOMTagType.SUBM);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fCharacterSet.TrimExcess();
            fFile.TrimExcess();
            fGEDCOM.TrimExcess();
            fNote.TrimExcess();
            fPlace.TrimExcess();
            fSource.TrimExcess();
            fSubmission.TrimExcess();
            fSubmitter.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            fCharacterSet.Clear();
            fCopyright = string.Empty;
            fFile.Clear();
            fGEDCOM.Clear();
            fLanguage = GDMLanguageID.Unknown;
            fNote.Clear();
            fPlace.Clear();
            fReceivingSystemName = string.Empty;
            fSource.Clear();
            fSubmission.Clear();
            fSubmitter.Clear();
            fTransmissionDateTime = new DateTime(0);
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fCharacterSet.IsEmpty() && string.IsNullOrEmpty(fCopyright) && fFile.IsEmpty() &&
                fGEDCOM.IsEmpty() && (fLanguage == GDMLanguageID.Unknown) && fNote.IsEmpty() && fPlace.IsEmpty() && 
                string.IsNullOrEmpty(fReceivingSystemName) && fSource.IsEmpty() && fSubmission.IsEmpty() &&
                fSubmitter.IsEmpty() && (fTransmissionDateTime.Equals(GDMChangeDate.ZeroDateTime));
        }
    }
}
