using System;
using System.IO;

namespace GedCom551
{
	public class TGEDCOMFileReference : TGEDCOMTag
	{
		public TGEDCOMMultimediaFormat MultimediaFormat
		{
			get { return GEDCOMUtils.GetMultimediaFormatVal(base.GetTagStringValue("FORM")); }
			set { base.SetTagStringValue("FORM", GEDCOMUtils.GetMultimediaFormatStr(value)); }
		}

		public TGEDCOMMediaType MediaType
		{
			get { return GEDCOMUtils.GetMediaTypeVal(base.GetTagStringValue(this.MediaTypeTagName())); }
			set { base.SetTagStringValue(this.MediaTypeTagName(), GEDCOMUtils.GetMediaTypeStr(value)); }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
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

		public static TGEDCOMMultimediaFormat RecognizeFormat(string fileName)
		{
            if (string.IsNullOrEmpty(fileName)) return TGEDCOMMultimediaFormat.mfUnknown;

            string ext = Path.GetExtension(fileName).ToLower();

			TGEDCOMMultimediaFormat result;
			if (ext == ".bmp")
			{
				result = TGEDCOMMultimediaFormat.mfBMP;
			}
			else if (ext == ".gif")
			{
				result = TGEDCOMMultimediaFormat.mfGIF;
			}
			else if (ext == ".jpg" || ext == ".jpeg")
			{
				result = TGEDCOMMultimediaFormat.mfJPG;
			}
			else if (ext == ".ole")
			{
				result = TGEDCOMMultimediaFormat.mfOLE;
			}
			else if (ext == ".pcx")
			{
				result = TGEDCOMMultimediaFormat.mfPCX;
			}
			else if (ext == ".tif" || ext == ".tiff")
			{
				result = TGEDCOMMultimediaFormat.mfTIF;
			}
			else if (ext == ".wav")
			{
				result = TGEDCOMMultimediaFormat.mfWAV;
			}
			else if (ext == ".txt")
			{
				result = TGEDCOMMultimediaFormat.mfTXT;
			}
			else if (ext == ".rtf")
			{
				result = TGEDCOMMultimediaFormat.mfRTF;
			}
			else if (ext == ".avi")
			{
				result = TGEDCOMMultimediaFormat.mfAVI;
			}
			else if (ext == ".tga")
			{
				result = TGEDCOMMultimediaFormat.mfTGA;
			}
			else if (ext == ".png")
			{
				result = TGEDCOMMultimediaFormat.mfPNG;
			}
			else if (ext == ".mpg" || ext == ".mpeg")
			{
				result = TGEDCOMMultimediaFormat.mfMPG;
			}
			else if (ext == ".htm" || ext == ".html")
			{
				result = TGEDCOMMultimediaFormat.mfHTM;
			}
			else
			{
				result = TGEDCOMMultimediaFormat.mfUnknown;
			}

			return result;
		}

		public TGEDCOMFileReference(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
