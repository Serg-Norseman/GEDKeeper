using System;
using System.IO;

namespace GedCom551
{
	public class TGEDCOMFileReference : TGEDCOMTag
	{
		public TGEDCOMMultimediaFormat MultimediaFormat
		{
			get { return GetMultimediaFormatVal(base.GetTagStringValue("FORM").Trim().ToUpper()); }
			set { base.SetTagStringValue("FORM", GetMultimediaFormatStr(value)); }
		}

		public TGEDCOMMediaType MediaType
		{
			get { return GetMediaTypeVal(base.GetTagStringValue(this.MediaTypeTagName()).Trim().ToUpper()); }
			set { base.SetTagStringValue(this.MediaTypeTagName(), GetMediaTypeStr(value)); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "FILE";
		}

		protected virtual string MediaTypeTagName()
		{
			return "FORM\\MEDI";
		}

		public void LinkFile(string AFile, TGEDCOMMediaType AMediaType, TGEDCOMMultimediaFormat AMultimediaFormat)
		{
			this.FStringValue = AFile;
			this.MultimediaFormat = RecognizeFormat(AFile);
			this.MediaType = AMediaType;
		}

		public static TGEDCOMMultimediaFormat RecognizeFormat(string AFile)
		{
			string E = Path.GetExtension(AFile).ToLower();
			TGEDCOMMultimediaFormat Result;
			if (E == ".bmp")
			{
				Result = TGEDCOMMultimediaFormat.mfBMP;
			}
			else if (E == ".gif")
			{
				Result = TGEDCOMMultimediaFormat.mfGIF;
			}
			else if (E == ".jpg" || E == ".jpeg")
			{
				Result = TGEDCOMMultimediaFormat.mfJPG;
			}
			else if (E == ".ole")
			{
				Result = TGEDCOMMultimediaFormat.mfOLE;
			}
			else if (E == ".pcx")
			{
				Result = TGEDCOMMultimediaFormat.mfPCX;
			}
			else if (E == ".tif" || E == ".tiff")
			{
				Result = TGEDCOMMultimediaFormat.mfTIF;
			}
			else if (E == ".wav")
			{
				Result = TGEDCOMMultimediaFormat.mfWAV;
			}
			else if (E == ".txt")
			{
				Result = TGEDCOMMultimediaFormat.mfTXT;
			}
			else if (E == ".rtf")
			{
				Result = TGEDCOMMultimediaFormat.mfRTF;
			}
			else if (E == ".avi")
			{
				Result = TGEDCOMMultimediaFormat.mfAVI;
			}
			else if (E == ".tga")
			{
				Result = TGEDCOMMultimediaFormat.mfTGA;
			}
			else if (E == ".png")
			{
				Result = TGEDCOMMultimediaFormat.mfPNG;
			}
			else if (E == ".mpg" || E == ".mpeg")
			{
				Result = TGEDCOMMultimediaFormat.mfMPG;
			}
			else if (E == ".htm" || E == ".html")
			{
				Result = TGEDCOMMultimediaFormat.mfHTM;
			}
			else
			{
				Result = TGEDCOMMultimediaFormat.mfUnknown;
			}
			return Result;
		}

		public TGEDCOMFileReference(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
