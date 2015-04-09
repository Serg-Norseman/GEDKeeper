using System.IO;
using GKCommon.GEDCOM.Enums;

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
			this.fName = "FILE";
		}

		protected virtual string MediaTypeTagName()
		{
			return "FORM\\MEDI";
		}

		public void LinkFile(string fileName)
		{
			this.fStringValue = fileName;
            this.MultimediaFormat = RecognizeFormat(fileName);
		}

		public static GEDCOMMultimediaFormat RecognizeFormat(string fileName)
		{
            if (string.IsNullOrEmpty(fileName)) return GEDCOMMultimediaFormat.mfUnknown;

            string ext = Path.GetExtension(fileName).ToLower();

			GEDCOMMultimediaFormat result;
			if (ext == ".bmp")
			{
				result = GEDCOMMultimediaFormat.mfBMP;
			}
			else if (ext == ".gif")
			{
				result = GEDCOMMultimediaFormat.mfGIF;
			}
			else if (ext == ".jpg" || ext == ".jpeg")
			{
				result = GEDCOMMultimediaFormat.mfJPG;
			}
			else if (ext == ".ole")
			{
				result = GEDCOMMultimediaFormat.mfOLE;
			}
			else if (ext == ".pcx")
			{
				result = GEDCOMMultimediaFormat.mfPCX;
			}
			else if (ext == ".tif" || ext == ".tiff")
			{
				result = GEDCOMMultimediaFormat.mfTIF;
			}
			else if (ext == ".wav")
			{
				result = GEDCOMMultimediaFormat.mfWAV;
			}
			else if (ext == ".txt")
			{
				result = GEDCOMMultimediaFormat.mfTXT;
			}
			else if (ext == ".rtf")
			{
				result = GEDCOMMultimediaFormat.mfRTF;
			}
			else if (ext == ".avi")
			{
				result = GEDCOMMultimediaFormat.mfAVI;
			}
			else if (ext == ".tga")
			{
				result = GEDCOMMultimediaFormat.mfTGA;
			}
			else if (ext == ".png")
			{
				result = GEDCOMMultimediaFormat.mfPNG;
			}
			else if (ext == ".mpg" || ext == ".mpeg")
			{
				result = GEDCOMMultimediaFormat.mfMPG;
			}
			else if (ext == ".htm" || ext == ".html")
			{
				result = GEDCOMMultimediaFormat.mfHTM;
			}
			else
			{
				result = GEDCOMMultimediaFormat.mfUnknown;
			}

			return result;
		}

		public GEDCOMFileReference(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
