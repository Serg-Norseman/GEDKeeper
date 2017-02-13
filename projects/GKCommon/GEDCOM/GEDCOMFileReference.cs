/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

namespace GKCommon.GEDCOM
{
    public class GEDCOMFileReference : GEDCOMTag
    {
        public GEDCOMMultimediaFormat MultimediaFormat
        {
            get { return GEDCOMUtils.GetMultimediaFormatVal(base.GetTagStringValue("FORM")); }
            set { base.SetTagStringValue("FORM", GEDCOMUtils.GetMultimediaFormatStr(value)); }
        }

        public GEDCOMMediaType MediaType
        {
            get { return GEDCOMUtils.GetMediaTypeVal(base.GetTagStringValue(this.MediaTypeTagName())); }
            set { base.SetTagStringValue(this.MediaTypeTagName(), GEDCOMUtils.GetMediaTypeStr(value)); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.SetName("FILE");
        }

        protected virtual string MediaTypeTagName()
        {
            return @"FORM\MEDI";
        }

        public void LinkFile(string fileName)
        {
            this.fStringValue = fileName;
            this.MultimediaFormat = RecognizeFormat(fileName);
        }

        public static GEDCOMMultimediaFormat RecognizeFormat(string fileName)
        {
            if (string.IsNullOrEmpty(fileName)) return GEDCOMMultimediaFormat.mfUnknown;

            string ext = SysUtils.GetFileExtension(fileName);
            if (!string.IsNullOrEmpty(ext) && ext[0] == '.') {
                ext = ext.Remove(0, 1);
            }

            GEDCOMMultimediaFormat result = GEDCOMUtils.GetMultimediaFormatVal(ext);
            return result;
        }

        public GEDCOMFileReference(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }
    }
}
