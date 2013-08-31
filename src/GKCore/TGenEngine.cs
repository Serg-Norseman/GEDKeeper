using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	public class TGenEngine : IDisposable
	{
		public const string AppTitle = "GEDKeeper2";

		private string FFileName;
		private TGEDCOMTree FTree;
		private bool Disposed_;


		public string FileName
		{
			get { return this.FFileName; }
			set { this.FFileName = value; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
		}

		public TGenEngine()
		{
			this.FTree = new TGEDCOMTree();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FTree.Dispose();
				this.Disposed_ = true;
			}
		}

		#region Data Manipulation

		public TGEDCOMSourceRecord aux_FindSource(string sourceName)
		{
			TGEDCOMSourceRecord result = null;
			int num = this.FTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = this.FTree[i];

				if (rec is TGEDCOMSourceRecord && (rec as TGEDCOMSourceRecord).FiledByEntry == sourceName)
				{
					result = (rec as TGEDCOMSourceRecord);
					break;
				}
			}
			return result;
		}

		public void aux_GetSourcesList(StringList aSources)
		{
			if (aSources != null)
			{
				aSources.Clear();
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMSourceRecord)
					{
						aSources.AddObject((rec as TGEDCOMSourceRecord).FiledByEntry, rec);
					}
				}
			}
		}

		public static void CleanFamily(TGEDCOMFamilyRecord aFamily)
		{
			if (aFamily != null)
			{
				int num = aFamily.Childrens.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMIndividualRecord child = aFamily.Childrens[i].Value as TGEDCOMIndividualRecord;
					child.DeleteChildToFamilyLink(aFamily);
				}

				TGEDCOMIndividualRecord spouse;

				spouse = aFamily.Husband.Value as TGEDCOMIndividualRecord;
				aFamily.aux_RemoveSpouse(spouse);

				spouse = (aFamily.Wife.Value as TGEDCOMIndividualRecord);
				aFamily.aux_RemoveSpouse(spouse);
			}
		}
		
		public static TGEDCOMCustomEvent CreateEventEx(TGEDCOMTree tree, TGEDCOMRecord aRec, string evSign, string evDate, string evPlace)
		{
			TGEDCOMCustomEvent Result;
			if (aRec is TGEDCOMIndividualRecord)
			{
				TGEDCOMIndividualRecord ind_rec = aRec as TGEDCOMIndividualRecord;
				if (TGenEngine.GetPersonEventKindBySign(evSign) == TPersonEventKind.ekEvent)
				{
					Result = new TGEDCOMIndividualEvent(tree, ind_rec, "", "");
				}
				else
				{
					Result = new TGEDCOMIndividualAttribute(tree, ind_rec, "", "");
				}
				ind_rec.AddIndividualEvent(Result);
			}
			else
			{
				if (!(aRec is TGEDCOMFamilyRecord))
				{
					Result = null;
					return Result;
				}
				TGEDCOMFamilyRecord fam_rec = aRec as TGEDCOMFamilyRecord;
				Result = new TGEDCOMFamilyEvent(tree, fam_rec, "", "");
				fam_rec.FamilyEvents.Add(Result as TGEDCOMFamilyEvent);
			}
			Result.Name = evSign;
			if (evDate != "")
			{
				Result.Detail.Date.ParseString(evDate);
			}
			if (evPlace != "")
			{
				Result.Detail.Place.StringValue = evPlace;
			}
			return Result;
		}

		public static TGEDCOMIndividualRecord CreatePersonEx(TGEDCOMTree tree, string iName, string iPatronymic, string iSurname, TGEDCOMSex iSex, bool birthEvent)
		{
			TGEDCOMIndividualRecord iRec = new TGEDCOMIndividualRecord(tree, tree, "", "");
			iRec.InitNew();
			iRec.Sex = iSex;
			iRec.ChangeDate.ChangeDateTime = DateTime.Now;

			TGEDCOMPersonalName pn = new TGEDCOMPersonalName(tree, iRec, "", "");
			pn.StringValue = iName.Trim() + " " + iPatronymic.Trim() + " /" + iSurname.Trim() + "/";
			iRec.AddPersonalName(pn);

			tree.AddRecord(iRec);

			if (birthEvent) TGenEngine.CreateEventEx(tree, iRec, "BIRT", "", "");

			return iRec;
		}

		public static void SetAddressValue(TGEDCOMAddress anAddress, string aValue)
		{
			StringList sl = new StringList(aValue);
			try
			{
				anAddress.Address = sl;
			}
			finally
			{
				sl.Free();
			}
		}

		public static void AddNoteText(TGEDCOMNoteRecord noteRec, string aText)
		{
			StringList strData = new StringList();
			try
			{
				strData.Text = noteRec.Note.Text.Trim();
				strData.Add(aText);
				noteRec.Note = strData;
			}
			finally
			{
				strData.Free();
			}
		}

		public static TGEDCOMNoteRecord CreateNoteEx(TGEDCOMTree tree, string text, TGEDCOMRecord toRecord)
		{
			TGEDCOMNoteRecord result = null;

			if (toRecord != null && !string.IsNullOrEmpty(text)) {
				result = tree.aux_CreateNote();
				AddNoteText(result, text);
				toRecord.aux_AddNote(result);
			}

			return result;
		}

		public static TGEDCOMNoteRecord CreateNoteEx(TGEDCOMTree tree, StringList text, TGEDCOMRecord toRecord)
		{
			TGEDCOMNoteRecord result = null;

			if (text != null) {
				result = tree.aux_CreateNote();
				result.Note = text;
			}

			if (toRecord != null && result != null) {
				toRecord.aux_AddNote(result);
			}
			
			return result;
		}

		#endregion

		#region Media Management

		public bool CheckPath()
		{
			string path = Path.GetDirectoryName(this.FFileName);

			bool result = (!string.IsNullOrEmpty(path));
			if (!result)
			{
				TGenEngine.ShowError("Для типов хранения \"архив\" и \"хранилище\" новый файл БД нужно предварительно сохранить");
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
			string result = Path.GetDirectoryName(this.FFileName) + "\\" + Path.GetFileNameWithoutExtension(this.FFileName) + ".zip";
			return result;
		}

		public string GetStgFolder(bool create)
		{
			string result = Path.GetDirectoryName(this.FFileName) + "\\" + Path.GetFileNameWithoutExtension(this.FFileName) + "\\";
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
			TGKStoreType gst = TGenEngine.GetStoreType(aRefName, ref target_fn);

			switch (gst) {
				case TGKStoreType.gstStorage:
					target_fn = this.GetStgFolder(false) + target_fn;
					if (!File.Exists(target_fn)) {
						if (throwException) {
							throw new MediaFileNotFoundException();
						} else {
							TGenEngine.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
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
							TGenEngine.ShowError(LangMan.LS(LSID.LSID_ArcNotFound));
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
				TGKStoreType gst = TGenEngine.GetStoreType(aRefName, ref target_fn);

				switch (gst) {
					case TGKStoreType.gstStorage:
						aFileName = this.GetStgFolder(false) + target_fn;
						break;

					case TGKStoreType.gstArchive:
						aFileName = TGenEngine.GetTempDir() + "\\" + Path.GetFileName(target_fn);
						FileStream fs = new FileStream(aFileName, FileMode.Create);
						try
						{
							if (!File.Exists(this.GetArcFileName())) {
								TGenEngine.ShowError(LangMan.LSList[476]);
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
				SysUtils.LogWrite("TGenEngine.MediaLoad_fn(): " + E.Message);
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
							TGenEngine.ShowError("Файл с таким именем уже есть в хранилище");
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
				SysUtils.LogWrite("TGenEngine.BitmapLoad(): " + E.Message);
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
				SysUtils.LogWrite("TGenEngine.GetPrimaryBitmap(): " + E.Message);
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
				SysUtils.LogWrite("TGenEngine.GetPrimaryBitmapStream(): " + E.Message);
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

		#endregion

		#region Aux functions

		public static TGEDCOMFormat GetGEDCOMFormat(TGEDCOMTree tree)
		{
			string sour = tree.Header.Source;

			TGEDCOMFormat res = TGEDCOMFormat.gf_Unknown;
			for (TGEDCOMFormat gf = TGEDCOMFormat.gf_Native; gf <= TGEDCOMFormat.gf_Last; gf++)
			{
				if (GKData.GEDCOMFormats[(int)gf].Sign == sour)
				{
					res = gf;
					break;
				}
			}
			return res;
		}

		public static string SexStr(TGEDCOMSex Sex)
		{
			return LangMan.LSList[(int)GKData.SexData[(int)Sex].NameId - 1];
		}

		public static TGEDCOMSex GetSexBySign(char SexSign)
		{
			TGEDCOMSex Result = TGEDCOMSex.svNone;
			
			switch (SexSign) {
				case 'F':
					Result = TGEDCOMSex.svFemale;
					break;
				case 'M':
					Result = TGEDCOMSex.svMale;
					break;
				case 'U':
					Result = TGEDCOMSex.svUndetermined;
					break;
			}
			
			return Result;
		}

		public static bool IsDevComp()
		{
			return (Environment.MachineName == "VALHALLA" || Environment.UserName == "Zhdanovskih_SV");
		}

		public static bool IsRecordAccess(TGEDCOMRestriction aRecRestriction, TShieldState aShieldState)
		{
			bool Result = false;

			switch (aShieldState) {
				case TShieldState.ssMaximum:
					Result = (((aRecRestriction == TGEDCOMRestriction.rnConfidential || aRecRestriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case TShieldState.ssMiddle:
					Result = (((aRecRestriction == TGEDCOMRestriction.rnPrivacy) ? 1 : 0) == 0);
					break;
				case TShieldState.ssNone:
					Result = true;
					break;
			}

			return Result;
		}

		public static string aux_GetFamilyStr(TGEDCOMFamilyRecord aFamily)
		{
			return aFamily.aux_GetFamilyStr(LangMan.LSList[64], LangMan.LSList[63]);
		}

		// FIXME: localization, old code
		public static TGEDCOMSex GetSex(string f_name, string f_pat, bool aQuery)
		{
			TGEDCOMSex Result = TGEDCOMSex.svNone;
			char c = f_name[((f_name != null) ? f_name.Length : 0) - 1];
			if (c != 'а')
			{
				if (c - 'в' < '\u0003' || c == 'й' || c - 'л' < '\u0006')
				{
					Result = TGEDCOMSex.svMale;
					goto IL_AE;
				}
				if (c != 'я')
				{
					goto IL_AE;
				}
			}
			if (((f_pat != null) ? f_pat.Length : 0) > 1)
			{
				char c2 = f_pat[((f_pat != null) ? f_pat.Length : 0) - 1];
				if (c2 == '0' || c2 == 'O')
				{
					Result = TGEDCOMSex.svFemale;
				}
				else
				{
					char c3 = f_pat[((f_pat != null) ? f_pat.Length : 0) - 1];
					if (c3 >= '2' && (c3 < '5' || c3 == '9' || (c3 >= ';' && c3 < 'A')))
					{
						Result = TGEDCOMSex.svMale;
					}
				}
			}
			IL_AE:
			if (aQuery && Result == TGEDCOMSex.svNone)
			{
				if (TGenEngine.ShowQuestion("Не определяется пол человека по имени \"" + f_name + " " + f_pat + "\". Это мужской пол?") == DialogResult.Yes)
				{
					Result = TGEDCOMSex.svMale;
				}
				else
				{
					Result = TGEDCOMSex.svFemale;
				}
			}
			return Result;
		}

		public static string SetAsName(string s)
		{
			string st = s.ToLower();
			char f = Char.ToUpper(st[0]);
			st = f + st.Substring(1);
			return st;
		}

		public static string ConStrings(StringList aStrings)
		{
			string Result = "";
			int num = aStrings.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (Result != "") Result += " ";
				Result += aStrings[i].Trim();
			}
			return Result;
		}

		public static string[] GetFamilies(TGEDCOMIndividualRecord iRec)
		{
			string[] result = new string[1];
			string fam, nam, pat;
			iRec.aux_GetNameParts(out fam, out nam, out pat);
			bool female = (iRec.Sex == TGEDCOMSex.svFemale);

			if (female) {
				fam = fam.Trim();
				int p = fam.IndexOf('(');
				if (p >= 0) {
					string part = fam.Substring(0, p).Trim();
					result[0] = PrepareRusSurname(part, female);
					part = fam.Substring(p).Trim();
					part = part.Substring(1, part.Length-2);

					string[] parts = part.Split(',');
					for (int i = 0; i < parts.Length; i++) {
						string[] newres = new string[result.Length+1];
						result.CopyTo(newres, 0);
						result = newres;
						result[result.Length-1] = PrepareRusSurname(parts[i].Trim(), female);
					}
				} else {
					result[0] = PrepareRusSurname(fam, female);
				}
			} else {
				result[0] = fam;
			}

			return result;
		}

		public static void GetLocationLinks(TGEDCOMTree tree, TGEDCOMLocationRecord locRec, ref StringList aList)
		{
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRecord rec = tree[i];
				if (rec is TGEDCOMIndividualRecord)
				{
					TGEDCOMIndividualRecord i_rec = rec as TGEDCOMIndividualRecord;
					int num2 = i_rec.IndividualEvents.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						TGEDCOMCustomEvent evt = i_rec.IndividualEvents[j];
						if (evt.Detail.Place.Location.Value == locRec)
						{
							aList.Add(TGenEngine.GenRecordLink(rec, true) + ", " + TGenEngine.GetEventName(evt).ToLower());
						}
					}
				}
				else
				{
					if (rec is TGEDCOMFamilyRecord)
					{
						TGEDCOMFamilyRecord f_rec = rec as TGEDCOMFamilyRecord;
						int num3 = f_rec.FamilyEvents.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							TGEDCOMCustomEvent evt = f_rec.FamilyEvents[j];
							if (evt.Detail.Place.Location.Value == locRec) {
								aList.Add(TGenEngine.GenRecordLink(rec, true) + ", " + TGenEngine.GetEventName(evt).ToLower());
							}
						}
					}
				}
			}
		}

		public static string HyperLink(string XRef, string Text, int Num)
		{
			string Result = "~^" + XRef;
			if (Text != "")
			{
				Result = Result + ":" + Text;
			}
			Result += "~";
			return Result;
		}

		public static string GenRecordLink(TGEDCOMRecord record, bool signed)
		{
            string result = "";

            if (record != null) {
				string sign = "";

                if (signed) {
					TGEDCOMRecordType recordType = record.RecordType;
					if (recordType != TGEDCOMRecordType.rtIndividual) {
						if (recordType == TGEDCOMRecordType.rtFamily || (byte)recordType - (byte)TGEDCOMRecordType.rtMultimedia < (byte)TGEDCOMRecordType.rtResearch)
						{
							sign = LangMan.LSList[(int)GKData.RecordTypes[(int)record.RecordType] - 1] + ": ";
						}
					} else {
						sign = "";
					}
				}

                string st;
				switch (record.RecordType) {
					case TGEDCOMRecordType.rtIndividual:
						st = (record as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
						break;
					case TGEDCOMRecordType.rtFamily:
						st = TGenEngine.aux_GetFamilyStr(record as TGEDCOMFamilyRecord);
						break;
					case TGEDCOMRecordType.rtMultimedia:
						st = (record as TGEDCOMMultimediaRecord).FileReferences[0].Title;
						break;
					case TGEDCOMRecordType.rtSource:
						st = (record as TGEDCOMSourceRecord).FiledByEntry;
						break;
					case TGEDCOMRecordType.rtRepository:
						st = (record as TGEDCOMRepositoryRecord).RepositoryName;
						break;
					case TGEDCOMRecordType.rtGroup:
						st = (record as TGEDCOMGroupRecord).GroupName;
						break;
					case TGEDCOMRecordType.rtResearch:
						st = (record as TGEDCOMResearchRecord).ResearchName;
						break;
					case TGEDCOMRecordType.rtTask:
						st = TGenEngine.GetTaskGoalStr(record as TGEDCOMTaskRecord);
						break;
					case TGEDCOMRecordType.rtCommunication:
						st = (record as TGEDCOMCommunicationRecord).CommName;
						break;
					case TGEDCOMRecordType.rtLocation:
						st = (record as TGEDCOMLocationRecord).LocationName;
						break;
					default:
						st = record.XRef;
						break;
				}

                result = TGenEngine.HyperLink(record.XRef, sign + st, 0);
			}

            return result;
		}

		public static string GetCorresponderStr(TGEDCOMTree tree, TGEDCOMCommunicationRecord commRec, bool aLink)
		{
			string Result = "";
			TCommunicationDir dir = TCommunicationDir.cdFrom;
			TGEDCOMIndividualRecord corresponder = null;
			commRec.GetCorresponder(ref dir, ref corresponder);
			if (corresponder != null)
			{
				string nm = corresponder.aux_GetNameStr(true, false);
				if (aLink)
				{
					nm = TGenEngine.HyperLink(corresponder.XRef, nm, 0);
				}
				Result = "[" + LangMan.LSList[(int)GKData.CommunicationDirs[(int)dir] - 1] + "] " + nm;
			}
			return Result;
		}

		public static string GetTaskGoalStr(TGEDCOMTaskRecord taskRec)
		{
			TGoalType gt = TGoalType.gtOther;
			TGEDCOMRecord tempRec = null;
            taskRec.aux_GetTaskGoal(ref gt, ref tempRec);
			string Result = "";

			switch (gt) {
				case TGoalType.gtIndividual:
					Result = (tempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false);
					break;
				case TGoalType.gtFamily:
					Result = TGenEngine.aux_GetFamilyStr(tempRec as TGEDCOMFamilyRecord);
					break;
				case TGoalType.gtSource:
					Result = (tempRec as TGEDCOMSourceRecord).FiledByEntry;
					break;
				case TGoalType.gtOther:
					Result = taskRec.Goal;
					break;
			}

			if (gt != TGoalType.gtOther)
			{
				Result = "[" + LangMan.LSList[(int)GKData.GoalNames[(int)gt] - 1] + "] " + Result;
			}
			return Result;
		}

		public static string ClearSurname(string surname)
		{
			int p = surname.IndexOf(" (");
			string result = ((p >= 0) ? surname.Substring(0, p) : surname);
			return result;
		}

		public static string PrepareRusSurname(string f, bool aFemale)
		{
			if (f == null || f.Length <= 0 || (f[0] == '(' && f[f.Length - 1] == ')'))
			{
				f = "?";
			}
			else
			{
				if (aFemale)
				{
					f = ClearSurname(f);

					if (f.EndsWith("а")) {
						f = f.Substring(0, f.Length - 1);
					} else if (f.EndsWith("кая")) {
						f = f.Substring(0, f.Length - 3) + "кий";
					} else if (f.EndsWith("ная")) {
						f = f.Substring(0, f.Length - 3) + "ный";
					}
				}
			}

			return f;
		}

		public static string GetRusWifeFamily(string husbFamily)
		{
			const string consonants = "бвгджзклмнпрстфхцчшщ";
			//const string vowels = "абвгдежзиклмнопрстуфхцчшщьыъэюя";
			
			string res;
			if (husbFamily == null || husbFamily.Length <= 0) {
				res = "?";
			} else {
				res = husbFamily;

				if (consonants.Contains(res[res.Length - 1])) {
					res = res + "а";
				} else if (res.EndsWith("кий")) {
					res = res.Substring(0, res.Length - 3) + "кая";
				} else if (res.EndsWith("ный")) {
					res = res.Substring(0, res.Length - 3) + "ная";
				}
			}

			return res;
		}

		#endregion

		#region Event Utils
		
		public static TGEDCOMCustomEvent GetIndividualEvent(TGEDCOMIndividualRecord iRec, string evName)
		{
			return ((iRec == null) ? null : iRec.GetIndividualEvent(evName));
		}

		public static string GetAttributeValue(TGEDCOMIndividualRecord iRec, string attrName)
		{
			TGEDCOMCustomEvent attr = TGenEngine.GetIndividualEvent(iRec, attrName);
			string result = ((attr == null) ? "" : attr.StringValue);
			return result;
		}

		public static TPersonEventKind GetPersonEventKindBySign(string aSign)
		{
			TPersonEventKind res = TPersonEventKind.ekFact;

			for (int i = 0; i < GKData.PersonEvents.Length; i++)
			{
				if (GKData.PersonEvents[i].Sign == aSign)
				{
					res = GKData.PersonEvents[i].Kind;
					break;
				}
			}

			return res;
		}

		public static int GetPersonEventIndex(string aSign)
		{
			int res = -1;

			for (int i = 0; i < GKData.PersonEvents.Length; i++)
			{
				if (GKData.PersonEvents[i].Sign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static int GetFamilyEventIndex(string aSign)
		{
			int res = -1;

			for (int i = 0; i < GKData.FamilyEvents.Length; i++)
			{
				if (GKData.FamilyEvents[i].Sign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static int GetMarriageStatusIndex(string aSign)
		{
			int res = 0;

			for (int i = 0; i < GKData.MarriageStatus.Length; i++)
			{
				if (GKData.MarriageStatus[i].StatSign == aSign)
				{
					res = i;
					break;
				}
			}

			return res;
		}

		public static string GetEventName(TGEDCOMCustomEvent aEvent)
		{
			string result = "";

			if (aEvent is TGEDCOMIndividualEvent || aEvent is TGEDCOMIndividualAttribute)
			{
				int ev = TGenEngine.GetPersonEventIndex(aEvent.Name);
				if (ev == 0) {
					result = aEvent.Detail.Classification;
				} else {
					if (ev > 0) {
						result = LangMan.LSList[(int)GKData.PersonEvents[ev].Name - 1];
					} else {
						result = aEvent.Name;
					}
				}
			}
			else if (aEvent is TGEDCOMFamilyEvent)
			{
				int ev = TGenEngine.GetFamilyEventIndex(aEvent.Name);
				if (ev == 0) {
					result = aEvent.Detail.Classification;
				} else {
					if (ev > 0) {
						result = LangMan.LSList[(int)GKData.FamilyEvents[ev].Name - 1];
					} else {
						result = aEvent.Name;
					}
				}
			}

			return result;
		}

		public static string GetAttributeStr(TGEDCOMIndividualAttribute iAttr)
		{
			int idx = TGenEngine.GetPersonEventIndex(iAttr.Name);
			string st;
			if (idx == 0)
			{
				st = iAttr.Detail.Classification;
			}
			else
			{
				if (idx > 0)
				{
					st = LangMan.LSList[(int)GKData.PersonEvents[idx].Name - 1];
				}
				else
				{
					st = iAttr.Name;
				}
			}

			string place = iAttr.Detail.Place.StringValue;
			if (place != "")
			{
				place = " [" + place + "]";
			}
			return st + ": " + iAttr.StringValue + place;
		}

		public static string GetEventDesc(TGEDCOMEventDetail eventDetail)
		{
			string dt = TGenEngine.GEDCOMCustomDateToStr(eventDetail.Date, TDateFormat.dfDD_MM_YYYY, false);
			string place = eventDetail.Place.StringValue;
			TGEDCOMLocationRecord location = eventDetail.Place.Location.Value as TGEDCOMLocationRecord;

			if (place != "" && location != null)
			{
				place = TGenEngine.HyperLink(location.XRef, place, 0);
			}

			string Result;

			if (dt == "" && place == "")
			{
				Result = "?";
			}
			else
			{
				if (dt == "")
				{
					Result = place;
				}
				else
				{
					if (place == "")
					{
						Result = dt;
					}
					else
					{
						Result = dt + ", " + place;
					}
				}
			}

			return Result;
		}

		public static string GetEventCause(TGEDCOMEventDetail eventDetail)
		{
			string result = "";

			if (eventDetail.Cause != "")
			{
				result += eventDetail.Cause;
			}

			if (eventDetail.Agency != "")
			{
				if (result != "")
				{
					result += " ";
				}
				result = result + "[" + eventDetail.Agency + "]";
			}

			return result;
		}

		#endregion

		#region Date functions

		public static string GEDCOMDateToStr(TGEDCOMDate aDate, TDateFormat aFormat)
		{
			string Result = "";
			int year;
			ushort month;
			ushort day;
			aDate.GetDate(out year, out month, out day);

			if (year > 0 || month > 0 || day > 0)
			{
				if (aFormat != TDateFormat.dfDD_MM_YYYY)
				{
					if (aFormat != TDateFormat.dfYYYY_MM_DD)
					{
						if (aFormat == TDateFormat.dfYYYY)
						{
							if (year > 0)
							{
								Result = year.ToString().PadLeft(4, '_');
							}
						}
					}
					else
					{
						if (year > 0)
						{
							Result = Result + year.ToString().PadLeft(4, '_') + ".";
						}
						else
						{
							Result += "____.";
						}
						if (month > 0)
						{
							Result = Result + SysUtils.NumUpdate((int)month, 2) + ".";
						}
						else
						{
							Result += "__.";
						}
						if (day > 0)
						{
							Result += SysUtils.NumUpdate((int)day, 2);
						}
						else
						{
							Result += "__";
						}
					}
				}
				else
				{
					if (day > 0)
					{
						Result = Result + SysUtils.NumUpdate((int)day, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (month > 0)
					{
						Result = Result + SysUtils.NumUpdate((int)month, 2) + ".";
					}
					else
					{
						Result += "__.";
					}
					if (year > 0)
					{
						Result += year.ToString().PadLeft(4, '_');
					}
					else
					{
						Result += "____";
					}
				}
			}
			return Result;
		}

		public static string StrToGEDCOMDate(string aDate, bool aException)
		{
			string Result = "";

			if (aDate.IndexOf("/") >= 0) aDate = aDate.Replace("/", ".");
			if (aDate.IndexOf("_") >= 0) aDate = aDate.Replace("_", " ");

			string[] dt_parts = aDate.Split('.');
			if (dt_parts.Length < 3)
			{
				if (aException)
				{
					throw new Exception("date failed");
				}
			}
			else
			{
				string pd = dt_parts[0].Trim();
				string pm = dt_parts[1].Trim();
				string py = dt_parts[2].Trim();

				if (pd != "") Result = Result + pd + " ";
				if (pm != "") Result = Result + TGEDCOMDate.GEDCOMMonthArray[SysUtils.ParseInt(pm, 1) - 1] + " ";
				if (py != "") Result += py;
			}
			return Result;
		}

		public static string GEDCOMCustomDateToStr(TGEDCOMDateValue dateValue, TDateFormat format, bool sign)
		{
			string result = "";

			TGEDCOMCustomDate date = dateValue.Value;

			if (date == null)
			{
				result = "";
			}
			else
			{
				if (date is TGEDCOMDateApproximated)
				{
					result = TGenEngine.GEDCOMDateToStr(date as TGEDCOMDate, format);
					if (sign && (date as TGEDCOMDateApproximated).Approximated != TGEDCOMApproximated.daExact)
					{
						result = "~ " + result;
					}
				}
				else
				{
					if (date is TGEDCOMDateRange)
					{
						TGEDCOMDateRange dt_range = date as TGEDCOMDateRange;
						if (dt_range.After.StringValue == "" && dt_range.Before.StringValue != "")
						{
							result = TGenEngine.GEDCOMDateToStr(dt_range.Before, format);
							if (sign)
							{
								result = "< " + result;
							}
						}
						else
						{
							if (dt_range.After.StringValue != "" && dt_range.Before.StringValue == "")
							{
								result = TGenEngine.GEDCOMDateToStr(dt_range.After, format);
								if (sign)
								{
									result += " >";
								}
							}
							else
							{
								if (dt_range.After.StringValue != "" && dt_range.Before.StringValue != "")
								{
									result = TGenEngine.GEDCOMDateToStr(dt_range.After, format) + "-" + TGenEngine.GEDCOMDateToStr(dt_range.Before, format);
								}
							}
						}
					}
					else
					{
						if (date is TGEDCOMDatePeriod)
						{
							TGEDCOMDatePeriod dt_period = date as TGEDCOMDatePeriod;
							if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue == "")
							{
								result = TGenEngine.GEDCOMDateToStr(dt_period.DateFrom, format);
								if (sign)
								{
									result += " >";
								}
							}
							else
							{
								if (dt_period.DateFrom.StringValue == "" && dt_period.DateTo.StringValue != "")
								{
									result = TGenEngine.GEDCOMDateToStr(dt_period.DateTo, format);
									if (sign)
									{
										result = "< " + result;
									}
								}
								else
								{
									if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue != "")
									{
										result = TGenEngine.GEDCOMDateToStr(dt_period.DateFrom, format) + "-" + TGenEngine.GEDCOMDateToStr(dt_period.DateTo, format);
									}
								}
							}
						}
						else
						{
							if (date is TGEDCOMDate)
							{
								result = TGenEngine.GEDCOMDateToStr(date as TGEDCOMDate, format);
							}
						}
					}
				}
			}

			if ((date is TGEDCOMDate) && (date as TGEDCOMDate).YearBC) {
				switch (format) {
					case TDateFormat.dfDD_MM_YYYY:
						result = result + " BC";
						break;
					case TDateFormat.dfYYYY_MM_DD:
						result = "BC " + result;
						break;
					case TDateFormat.dfYYYY:
						result = "BC " + result;
						break;
				}
			}

			return result;
		}

		public static string GEDCOMEventToDateStr(TGEDCOMCustomEvent aEvent, TDateFormat format, bool sign)
		{
			return ((aEvent == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(aEvent.Detail.Date, format, sign));
		}

		public static DateTime GEDCOMDateToDate(TGEDCOMDateValue date)
		{
			return ((date == null) ? new DateTime(0) : date.aux_GetDate());
		}

		public static string CompactDate(string date)
		{
			string result = date;
			while (result.IndexOf("__.") == 0) result = result.Remove(0, 3);
			return result;
		}

		public static string GetBirthDate(TGEDCOMIndividualRecord iRec, TDateFormat dateFormat, bool compact)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
			string result = ((evt == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = TGenEngine.CompactDate(result);
			return result;
		}

		public static string GetDeathDate(TGEDCOMIndividualRecord iRec, TDateFormat dateFormat, bool compact)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "DEAT");
			string result = ((evt == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			if (compact) result = TGenEngine.CompactDate(result);
			return result;
		}

		public static string GetLifeStr(TGEDCOMIndividualRecord iRec)
		{
			string Result = " (";

			string ds = TGenEngine.GetBirthDate(iRec, TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				ds = "?";
			}
			Result += ds;

			ds = TGenEngine.GetDeathDate(iRec, TDateFormat.dfDD_MM_YYYY, false);
			if (ds == "")
			{
				TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, "DEAT");
				if (ev != null)
				{
					ds = "?";
				}
			}

			if (ds != "")
			{
				Result = Result + " - " + ds;
			}

			Result += ")";
			return Result;
		}

		// FIXME: aux_candidate
		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign)
		{
			bool dummy;
			return GetIndependentYear(iRec, evSign, out dummy);
		}

		public static int GetIndependentYear(TGEDCOMIndividualRecord iRec, string evSign, out bool YearBC)
		{
			int Result = -1;
			YearBC = false;

			TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, evSign);
			if (ev != null)
			{
				int year;
				ushort am, ad;
				ev.Detail.Date.aux_GetIndependentDate(out year, out am, out ad, out YearBC);
				Result = year;
			}
			return Result;
		}

		public static double GetAbstractDate(TGEDCOMEventDetail eventDetail)
		{
			bool dummy;
			return GetAbstractDate(eventDetail, out dummy);
		}

		public static double GetAbstractDate(TGEDCOMEventDetail eventDetail, out bool YearBC)
		{
			double result = 0.0;
			YearBC = false;

			if (eventDetail != null)
			{
				int y;
				ushort i;
				ushort d;
				eventDetail.Date.aux_GetIndependentDate(out y, out i, out d, out YearBC);
				if (y > 0)
				{
					result = (double)y;
					if (i > 0)
					{
						result = (result + i / 12.0);
						if (d > 0)
						{
							result = (result + d / SysUtils.DaysInAMonth((ushort)y, i) / 12.0);
						}
					}
				}
			}

			return result;
		}

		public static string GetEventsYearsDiff(TGEDCOMCustomEvent ev1, TGEDCOMCustomEvent ev2, bool aCurEnd)
		{
			string result = "?";

			try
			{
				bool ybc, ybc2;
				double y = ((ev1 == null) ? -1.0 : TGenEngine.GetAbstractDate(ev1.Detail, out ybc));
				double y2 = ((ev2 == null) ? -1.0 : TGenEngine.GetAbstractDate(ev2.Detail, out ybc2));

				if (aCurEnd && y2 <= (double)1f)
				{
					y2 = ((double)DateTime.Now.Year + (double)DateTime.Now.Month / 12.0);
				}

				if (y == (double)-1f || y2 == (double)-1f)
				{
					result = "";
				}
				else
				{
					if (y == (double)0f || y2 == (double)0f)
					{
						result = "?";
					}
					else
					{
						long delta = SysUtils.Trunc(y2 - y);
						result = delta.ToString();
					}
				}

				//if ()
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetEventsYearsDiff(): " + E.Message);
			}

			return result;
		}

		public static string GetLifeExpectancy(TGEDCOMIndividualRecord iRec)
		{
			string result = "";

			try
			{
				TGEDCOMCustomEvent ev = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					if (evt.Name == "BIRT")
					{
						ev = evt;
					}
					else if (evt.Name == "DEAT")
					{
						ev2 = evt;
					}
				}

				result = TGenEngine.GetEventsYearsDiff(ev, ev2, false);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetLifeExpectancy(): " + E.Message);
			}

			return result;
		}

		public static string GetAge(TGEDCOMIndividualRecord iRec, int ToYear)
		{
			string result = "";

			try
			{
				TGEDCOMCustomEvent ev1 = null;
				TGEDCOMCustomEvent ev2 = null;

				int num = iRec.IndividualEvents.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMCustomEvent evt = iRec.IndividualEvents[i];
					if (evt.Name == "BIRT" && ev1 == null)
					{
						ev1 = evt;
					}
					else
					{
						if (evt.Name == "DEAT" && ev2 == null)
						{
							ev2 = evt;
						}
					}
				}

				if (ToYear == -1)
				{
					result = TGenEngine.GetEventsYearsDiff(ev1, ev2, ev2 == null);
				}
				else
				{
					if (ev1 == null)
					{
						result = "";
					}
					else
					{
						ushort dummy;
						int i;
						ev1.Detail.Date.aux_GetIndependentDate(out i, out dummy, out dummy);
						result = Convert.ToString(ToYear - i);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetAge(): " + E.Message);
			}

			return result;
		}

		public static string GetMarriageDate(TGEDCOMFamilyRecord fRec, TDateFormat dateFormat)
		{
			string result = "";

			if (fRec != null)
			{
				TGEDCOMFamilyEvent evt = fRec.aux_GetFamilyEvent("MARR");
				result = ((evt == null) ? "" : TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, dateFormat, false));
			}

			return result;
		}

		public static string GetDaysForBirth(TGEDCOMIndividualRecord iRec)
		{
			string Result = "";
			try
			{
				TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "DEAT");
				if (evt != null)
				{
				}
				else
				{
					evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
					if (evt != null)
					{
						TGEDCOMDate dt = evt.Detail.Date.Value as TGEDCOMDate;
						if (dt != null)
						{
							int bd_y;
							ushort bd_m;
							ushort bd_d;

							dt.GetDate(out bd_y, out bd_m, out bd_d);
							if (bd_m <= 0 || bd_d <= 0)
							{
							}
							else
							{
								DateTime dtx = DateTime.Now;
								ushort cur_y = (ushort)dtx.Year;
								ushort cur_m = (ushort)dtx.Month;
								ushort cur_d = (ushort)dtx.Day;
								double dt2 = (cur_y + bd_m / 12.0 + bd_d / 12.0 / 31.0);
								double dt3 = (cur_y + cur_m / 12.0 + cur_d / 12.0 / 31.0);
								if (dt2 < dt3)
								{
									bd_y = (int)(cur_y + 1u);
								}
								else
								{
									bd_y = (int)cur_y;
								}
								Result = Convert.ToString(SysUtils.DaysBetween(new DateTime((int)cur_y, (int)cur_m, (int)cur_d), new DateTime(bd_y, (int)bd_m, (int)bd_d)));
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.GetDaysForBirth(): " + E.Message);
			}
			return Result;
		}

		#endregion

		#region Places functions

		public static string GetBirthPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "BIRT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetDeathPlace(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMCustomEvent evt = TGenEngine.GetIndividualEvent(iRec, "DEAT");
			string result = ((evt == null) ? "" : evt.Detail.Place.StringValue);
			return result;
		}

		public static string GetResidencePlace(TGEDCOMIndividualRecord iRec, bool includeAddress)
		{
			return TGenEngine.GetPlaceStr(TGenEngine.GetIndividualEvent(iRec, "RESI"), includeAddress);
		}

		public static string GetPlaceStr(TGEDCOMCustomEvent aEvent, bool includeAddress)
		{
			string Result;
			if (aEvent == null)
			{
				Result = "";
			}
			else
			{
				Result = aEvent.Detail.Place.StringValue;
				if (includeAddress)
				{
					string resi = aEvent.StringValue;
					string addr = aEvent.Detail.Address.Address.Text.Trim();
					if (resi != "" && addr != "")
					{
						resi += ", ";
					}
					resi += addr;
					if (resi != "")
					{
						Result = Result + " [" + resi + "]";
					}
				}
			}
			return Result;
		}

		#endregion

		#region Match functions

		public static Regex InitMaskRegex(string Mask)
		{
			Regex result = null;

			if  (!string.IsNullOrEmpty(Mask))
			{
				string regex_str = "";
				int CurPos = 0;
				int Len = Mask.Length;
				if (CurPos < Len)
				{
					do
					{
						int I = Mask.IndexOfAny("*?".ToCharArray(), CurPos);
						if (I < CurPos) break;
						if (I > CurPos) {
							string part = Mask.Substring(CurPos, I - CurPos);
							regex_str += Regex.Escape(part);
						}

						char c = Mask[I];
						switch (c) {
							case '*':
								regex_str += ".*";
								break;
							case '?':
								regex_str += ".";
								break;
						}

						CurPos = I + 1;
					}
					while (CurPos < Len);
				}

				if (CurPos < Len) {
					string part = Mask.Substring(CurPos, Len - CurPos);
					regex_str += Regex.Escape(part);
				}

				result = new Regex(regex_str, RegexOptions.IgnoreCase);
			}

			return result;
		}

		public static bool MatchesRegex(string S, Regex regex)
		{
			return ((regex != null) ? regex.IsMatch(S) : false);
		}

		public static bool MatchesMask(string S, string Mask)
		{
			Regex regex = InitMaskRegex(Mask);
			return MatchesRegex(S, regex);
		}

		#endregion

		#region Folder functions

		public static string GetTempDir()
		{
			return Environment.GetEnvironmentVariable("TEMP");
		}

		public static string GetAppPath()
		{
			Module[] mods = System.Reflection.Assembly.GetExecutingAssembly().GetModules();
			string fn = mods[0].FullyQualifiedName;
			return Path.GetDirectoryName(fn) + "\\";
		}

		public static string GetAppDataPath()
		{
			string path = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + "\\" + TGenEngine.AppTitle + "\\";
			if (!Directory.Exists(path)) Directory.CreateDirectory(path);
			return path;
		}

		#endregion

		#region UI functions

		public static void ShowMessage(string Msg)
		{
			MessageBox.Show(Msg, AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
		}

		public static void ShowError(string Msg)
		{
			MessageBox.Show(Msg, AppTitle, MessageBoxButtons.OK, MessageBoxIcon.Hand);
		}

		public static DialogResult ShowQuestion(string Msg)
		{
			return MessageBox.Show(Msg, AppTitle, MessageBoxButtons.YesNo, MessageBoxIcon.Question);
		}

		#endregion

	}
}
