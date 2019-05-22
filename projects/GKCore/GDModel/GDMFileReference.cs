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

using BSLib;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GEDCOMMediaType
    {
        mtUnknown,

        mtAudio,
        mtBook,
        mtCard,
        mtElectronic,
        mtFiche,
        mtFilm,
        mtMagazine,
        mtManuscript,
        mtMap,
        mtNewspaper,
        mtPhoto,
        mtTombstone,
        mtVideo,

        mtLast = mtVideo
    }


    // TODO: doc/docx/odt, xls/xlsx/ods, ppt/pptx/odp
    public enum GEDCOMMultimediaFormat
    {
        mfNone,

        mfBMP,
        mfGIF,
        mfJPG,
        mfPCX,
        mfTIF,
        mfTGA,
        mfPNG,
        mfRAW,
        mfPSD,

        mfTXT,
        mfRTF,
        mfHTM,
        mfPDF,

        mfWAV,
        mfMP3,
        mfWMA,
        mfMKA,

        mfAVI,
        mfMPG,
        mfWMV,
        mfMP4,
        mfOGV,
        mfMKV,
        mfMOV,

        mfOLE,
        mfUnknown
    }


    public class GDMFileReference : GDMTag
    {
        public GEDCOMMultimediaFormat MultimediaFormat
        {
            get { return GEDCOMUtils.GetMultimediaFormatVal(GetTagStringValue(GEDCOMTagType.FORM)); }
            set { SetTagStringValue(GEDCOMTagType.FORM, GEDCOMUtils.GetMultimediaFormatStr(value)); }
        }

        public GEDCOMMediaType MediaType
        {
            get { return GEDCOMUtils.GetMediaTypeVal(GetTagStringValue(MediaTypeTagName())); }
            set { SetTagStringValue(MediaTypeTagName(), GEDCOMUtils.GetMediaTypeStr(value)); }
        }


        public GDMFileReference(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.FILE);
        }

        public GDMFileReference(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        protected virtual string MediaTypeTagName()
        {
            return @"FORM\MEDI";
        }

        public void LinkFile(string fileName)
        {
            fStringValue = fileName;
            MultimediaFormat = RecognizeFormat(fileName);
        }

        public static GEDCOMMultimediaFormat RecognizeFormat(string fileName)
        {
            if (string.IsNullOrEmpty(fileName)) return GEDCOMMultimediaFormat.mfUnknown;

            string ext = FileHelper.GetFileExtension(fileName);
            if (!string.IsNullOrEmpty(ext) && ext[0] == '.') {
                ext = ext.Remove(0, 1);
            }

            GEDCOMMultimediaFormat result = GEDCOMUtils.GetMultimediaFormatVal(ext);
            return result;
        }
    }
}
