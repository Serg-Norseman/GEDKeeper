using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.IO.Compression;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public sealed class MediaManager
	{
		private TGEDCOMTree FTree;

		public MediaManager(TGEDCOMTree tree)
		{
			this.FTree = tree;
		}

		public bool CheckPath()
		{
			string path = Path.GetDirectoryName(this.FTree.FileName);

			bool result = (!string.IsNullOrEmpty(path));
			if (!result)
			{
				GKUtils.ShowError("Для типов хранения \"архив\" и \"хранилище\" новый файл БД нужно предварительно сохранить");
			}
			return result;
		}

		public string GetContainerName(string fileName, bool arc)
		{
			string result = Path.GetFileNameWithoutExtension(fileName);
			if (arc) {
				result += ".zip";
			} else {
				result += "\\";
			}
			return result;
		}

		public string GetArcFileName()
		{
			string treeName = this.FTree.FileName;
			string result = Path.GetDirectoryName(treeName) + "\\" + Path.GetFileNameWithoutExtension(treeName) + ".zip";
			return result;
		}

		public string GetStgFolder(bool create)
		{
			string treeName = this.FTree.FileName;
			string result = Path.GetDirectoryName(treeName) + "\\" + Path.GetFileNameWithoutExtension(treeName) + "\\";
			if (!Directory.Exists(result) && create) Directory.CreateDirectory(result);
			return result;
		}

		public static TGKStoreType GetStoreType(string fileRef, ref string fileName)
		{
			fileName = fileRef;
			TGKStoreType result;

			if (fileRef.IndexOf(GKData.GKStoreTypes[2].Sign) == 0)
			{
				result = TGKStoreType.gstArchive;
				fileName = fileName.Remove(0, 4);
			}
			else
			{
				if (fileRef.IndexOf(GKData.GKStoreTypes[1].Sign) == 0)
				{
					result = TGKStoreType.gstStorage;
					fileName = fileName.Remove(0, 4);
				}
				else
				{
					result = TGKStoreType.gstReference;
				}
			}

			return result;
		}

		public void MediaLoad(string aRefName, out Stream aStream, bool throwException)
		{
			aStream = null;
			string target_fn = "";
			TGKStoreType gst = MediaManager.GetStoreType(aRefName, ref target_fn);

			switch (gst) {
				case TGKStoreType.gstStorage:
					target_fn = this.GetStgFolder(false) + target_fn;
					if (!File.Exists(target_fn)) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
						}
					} else {
						aStream = new FileStream(target_fn, FileMode.Open);
					}
					break;

				case TGKStoreType.gstArchive:
					aStream = new MemoryStream();
					if (!File.Exists(this.GetArcFileName())) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							GKUtils.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
						}
					} else {
						this.ArcFileLoad(target_fn, aStream);
						aStream.Seek((long)0, SeekOrigin.Begin);
					}
					break;

				case TGKStoreType.gstReference:
					aStream = new FileStream(target_fn, FileMode.Open);
					break;
			}
		}

		public void MediaLoad(string aRefName, ref string aFileName)
		{
			try
			{
				string target_fn = "";
				TGKStoreType gst = MediaManager.GetStoreType(aRefName, ref target_fn);

				switch (gst) {
					case TGKStoreType.gstStorage:
						aFileName = this.GetStgFolder(false) + target_fn;
						break;

					case TGKStoreType.gstArchive:
						aFileName = GKUtils.GetTempDir() + "\\" + Path.GetFileName(target_fn);
						FileStream fs = new FileStream(aFileName, FileMode.Create);
						try
						{
							if (!File.Exists(this.GetArcFileName())) {
								GKUtils.ShowError(LangMan.LSList[476]);
							} else {
								target_fn = target_fn.Replace("\\", "/");
								this.ArcFileLoad(target_fn, fs);
							}
						}
						finally
						{
							fs.Close();
							fs.Dispose();
						}
						break;

					case TGKStoreType.gstReference:
						aFileName = target_fn;
						break;
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.MediaLoad_fn(): " + E.Message);
				aFileName = "";
			}
		}

		public bool MediaSave(string aFileName, TGKStoreType storeType, ref string aRefPath)
		{
			bool result = true;

			string sfn = Path.GetFileName(aFileName);
			string spath = "";

			switch (TGEDCOMFileReference.RecognizeFormat(aFileName))
			{
				case TGEDCOMMultimediaFormat.mfNone:
				case TGEDCOMMultimediaFormat.mfOLE:
				case TGEDCOMMultimediaFormat.mfUnknown:
					spath = "unknown\\";
					break;

				case TGEDCOMMultimediaFormat.mfBMP:
				case TGEDCOMMultimediaFormat.mfGIF:
				case TGEDCOMMultimediaFormat.mfJPG:
				case TGEDCOMMultimediaFormat.mfPCX:
				case TGEDCOMMultimediaFormat.mfTIF:
				case TGEDCOMMultimediaFormat.mfTGA:
				case TGEDCOMMultimediaFormat.mfPNG:
					spath = "images\\";
					break;

				case TGEDCOMMultimediaFormat.mfWAV:
					spath = "audio\\";
					break;

				case TGEDCOMMultimediaFormat.mfTXT:
				case TGEDCOMMultimediaFormat.mfRTF:
				case TGEDCOMMultimediaFormat.mfHTM:
					spath = "texts\\";
					break;

				case TGEDCOMMultimediaFormat.mfAVI:
				case TGEDCOMMultimediaFormat.mfMPG:
					spath = "video\\";
					break;
			}

			switch (storeType) {
				case TGKStoreType.gstReference:
					{
						aRefPath = aFileName;
						break;
					}

				case TGKStoreType.gstArchive:
					{
						sfn = spath + sfn;
						aRefPath = GKData.GKStoreTypes[(int)storeType].Sign + sfn;
						this.ArcFileSave(aFileName, sfn);
						break;
					}

				case TGKStoreType.gstStorage:
					{
						string target_dir = this.GetStgFolder(true) + spath;
						string target_fn = target_dir + sfn;

						aRefPath = GKData.GKStoreTypes[(int)storeType].Sign + spath + sfn;

						if (!Directory.Exists(target_dir)) Directory.CreateDirectory(target_dir);
						try
						{
							File.Copy(aFileName, target_fn, false);
						}
						catch (IOException)
						{
							GKUtils.ShowError("Файл с таким именем уже есть в хранилище");
							result = false;
						}

						break;
					}
			}

			return result;
		}

		public Bitmap BitmapLoad(string media, int thumbWidth, int thumbHeight, bool throwException)
		{
			Bitmap result = null;
			try
			{
				Stream stm;
				this.MediaLoad(media, out stm, throwException);
				if (stm != null)
				{
					if (stm.Length != 0) {
						using (Bitmap bmp = new Bitmap(stm))
						{
							int new_width;
							int new_height;

							if (thumbWidth > 0 && thumbHeight > 0)
							{
								int maxSize_src = ((bmp.Height > bmp.Width) ? bmp.Height : bmp.Width);
								int minSize_dst = ((thumbHeight < thumbWidth) ? thumbHeight : thumbWidth);
								double ratio = (double)minSize_dst / maxSize_src;
								new_width = (int)(bmp.Width * ratio);
								new_height = (int)(bmp.Height * ratio);
							} else {
								new_width = bmp.Width;
								new_height = bmp.Height;
							}

							Bitmap new_image = new Bitmap(new_width, new_height, PixelFormat.Format24bppRgb);
							Graphics graphic = Graphics.FromImage(new_image);
							graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
							graphic.SmoothingMode = SmoothingMode.HighQuality;
							graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
							graphic.CompositingQuality = CompositingQuality.HighQuality;
							graphic.DrawImage(bmp, 0, 0, new_width, new_height);

							result = new_image;
						}
					}
					stm.Dispose();
				}
			}
			catch (MediaFileNotFoundException ex)
			{
				throw ex;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.BitmapLoad(): " + E.Message);
				result = null;
			}
			return result;
		}

		public Bitmap GetPrimaryBitmap(TGEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException)
		{
			Bitmap result = null;
			try
			{
				TGEDCOMMultimediaLink mmLink = iRec.aux_GetPrimaryMultimediaLink();
				if (mmLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					result = this.BitmapLoad(mmRec.FileReferences[0].StringValue, thumbWidth, thumbHeight, throwException);
				}
			}
			catch (MediaFileNotFoundException ex)
			{
				throw ex;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.GetPrimaryBitmap(): " + E.Message);
				result = null;
			}
			return result;
		}

		public Stream GetPrimaryBitmapStream(TGEDCOMIndividualRecord aRec)
		{
			Stream result = null;
			try
			{
				TGEDCOMMultimediaLink mmLink = aRec.aux_GetPrimaryMultimediaLink();
				if (mmLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					this.MediaLoad(mmRec.FileReferences[0].StringValue, out result, false);
				}
			}
			catch (MediaFileNotFoundException ex)
			{
				throw ex;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKUtils.GetPrimaryBitmapStream(): " + E.Message);
				result = null;
			}
			return result;
		}

		public void ArcFileLoad(string target_fn, Stream toStream)
		{
			// http://www.icsharpcode.net/OpenSource/SharpZipLib/ - slow, but high compression ratio
			// http://dotnetzip.codeplex.com/ - fast, but low compression ratio

			target_fn = target_fn.Replace('\\', '/');

			using (ZipStorer zip = ZipStorer.Open(this.GetArcFileName(), FileAccess.Read))
			{
				List<ZipStorer.ZipFileEntry> dir = zip.ReadCentralDir();
				foreach (ZipStorer.ZipFileEntry entry in dir)
				{
					if (entry.FilenameInZip.Equals(target_fn)) {
						zip.ExtractFile(entry, toStream);
						break;
					}
				}
			}
		}

		public void ArcFileSave(string aFileName, string sfn)
		{
			string arc_fn = this.GetArcFileName();
			ZipStorer zip = null;

			try
			{
				if (File.Exists(arc_fn)) {
					zip = ZipStorer.Open(arc_fn, FileAccess.ReadWrite);
				} else {
					zip = ZipStorer.Create(arc_fn, "");
				}
				zip.AddFile(ZipStorer.Compression.Deflate, aFileName, sfn, null);
			}
			finally
			{
				if (zip != null) zip.Dispose();
			}
		}

	}
}
