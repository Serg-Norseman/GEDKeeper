/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMMediaType
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


    public enum GDMMultimediaFormat
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
        mfWEBP,

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

        mfDJVU,
        mfDOC,
        mfDOCX,
        mfXLS,
        mfXLSX,
        mfPPT,
        mfPPTX,
        mfODT,
        mfODS,
        mfODP,

        mfZIP,
        mfRAR,
        mf7Z,

        mfOLE,
        mfUnknown
    }


    public class GDMFileReference : GDMValueTag
    {
        private string fMultimediaFormat;
        private GDMMediaType fMediaType;


        public string MultimediaFormat
        {
            get { return fMultimediaFormat; }
            set { fMultimediaFormat = value; }
        }

        public GDMMediaType MediaType
        {
            get { return fMediaType; }
            set { fMediaType = value; }
        }


        public GDMFileReference()
        {
            SetName(GEDCOMTagType.FILE);

            fMultimediaFormat = string.Empty;
            fMediaType = GDMMediaType.mtUnknown;
        }

        public override void Assign(GDMTag source)
        {
            GDMFileReference sourceObj = (source as GDMFileReference);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", nameof(source));

            base.Assign(sourceObj);

            fMultimediaFormat = sourceObj.fMultimediaFormat;
            fMediaType = sourceObj.fMediaType;
        }

        public override void Clear()
        {
            base.Clear();

            fMultimediaFormat = string.Empty;
            fMediaType = GDMMediaType.mtUnknown;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fMultimediaFormat) && (fMediaType == GDMMediaType.mtUnknown);
        }

        public void LinkFile(string fileName)
        {
            fStringValue = fileName;
            fMultimediaFormat = GetMultimediaExt(fileName);
        }

        public GDMMultimediaFormat GetMultimediaFormat()
        {
            return RecognizeFormat(fStringValue);
        }

        public static string GetMultimediaExt(string fileName)
        {
            if (string.IsNullOrEmpty(fileName)) return string.Empty;

            string ext = FileHelper.GetFileExtension(fileName);
            if (!string.IsNullOrEmpty(ext) && ext[0] == '.') {
                ext = ext.Remove(0, 1);
            }
            return ext;
        }

        public static GDMMultimediaFormat RecognizeFormat(string fileName)
        {
            // GetMultimediaExt() -> GetFileExtension() already returns in lowercase
            var strFmt = GetMultimediaExt(fileName);
            return string.IsNullOrEmpty(strFmt) ? GDMMultimediaFormat.mfUnknown : GEDCOMUtils.GetMultimediaFormatVal(strFmt);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fMultimediaFormat);
            hashCode.Add(fMediaType);
        }
    }
}
