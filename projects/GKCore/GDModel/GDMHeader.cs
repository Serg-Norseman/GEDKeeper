/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    // TODO: remove and work with string idents
    public enum GEDCOMCharacterSet
    {
        csASCII,
        csANSEL,
        csUNICODE,
        csUTF8
    }


    public sealed class GDMHeaderSource : GDMTag
    {
        public string Version { get; set; }
        public string ProductName { get; set; }


        public GDMHeaderSource(GDMObject owner) : base(owner)
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


        public GDMHeaderGEDCOM(GDMObject owner) : base(owner)
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
        public GEDCOMCharacterSet Value
        {
            get { return GEDCOMUtils.GetCharacterSetVal(StringValue); }
            set { StringValue = GEDCOMUtils.GetCharacterSetStr(value); }
        }

        public string Version { get; set; }


        public GDMHeaderCharSet(GDMObject owner) : base(owner)
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
    }


    public sealed class GDMHeaderFile : GDMTag
    {
        public int Revision { get; set; }


        public GDMHeaderFile(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.FILE);
        }

        public override void Clear()
        {
            base.Clear();

            Revision = 0;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (Revision == 0);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMHeader : GDMTag
    {
        private GDMHeaderCharSet fCharacterSet;
        private string fCopyright;
        private GDMHeaderFile fFile;
        private GDMHeaderGEDCOM fGEDCOM;
        private GDMLanguageID fLanguage;
        private GDMTextTag fNote;
        private GDMPlace fPlace;
        private string fReceivingSystemName;
        private GDMHeaderSource fSource;
        private GDMPointer fSubmission;
        private GDMPointer fSubmitter;
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


        public GDMHeader(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.HEAD);

            fCharacterSet = new GDMHeaderCharSet(this);
            fFile = new GDMHeaderFile(this);
            fGEDCOM = new GDMHeaderGEDCOM(this);
            fNote = new GDMTextTag(this, (int)GEDCOMTagType.NOTE);
            fPlace = new GDMPlace(this);
            fSource = new GDMHeaderSource(this);
            fSubmission = new GDMPointer(this, (int)GEDCOMTagType.SUBN, string.Empty);
            fSubmitter = new GDMPointer(this, (int)GEDCOMTagType.SUBM, string.Empty);
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
