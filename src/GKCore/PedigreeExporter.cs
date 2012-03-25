using System;
using System.IO;
using System.Text;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKCore
{
	// FIXME: вычистить код
	public class PedigreeExporter : HTMLExporter
	{
		private class TPersonObj
		{
			public TPersonObj Parent;
			public string Id;
			public TGEDCOMIndividualRecord iRec;
			public int Level;
			public string BirthDate;
			public string Sources;
			public int FamilyOrder;
			public int ChildIdx;

			public string GetOrderStr()
			{
				char order = (char)this.FamilyOrder;
				string Result = ((this.Parent == null) ? new string(order, 1) : this.Parent.GetOrderStr() + order);
				return Result;
			}

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		private class TEventObj
		{
			public TGEDCOMCustomEvent Event;
			public TGEDCOMIndividualRecord iRec;

			public TEventObj(TGEDCOMCustomEvent aEvent, TGEDCOMIndividualRecord aRec)
			{
				this.Event = aEvent;
				this.iRec = aRec;
			}

			public DateTime GetDate()
			{
				DateTime Result = ((this.Event == null) ? new DateTime(0) : TGenEngine.GEDCOMDateToDate(this.Event.Detail.Date));
				return Result;
			}

			public void Free()
			{
				SysUtils.Free(this);
			}

		}

		public enum TPedigreeKind : byte
		{
			pk_dAboville,
			pk_Konovalov
		}

		private TGEDCOMIndividualRecord FAncestor;
		private TPedigreeKind FKind;
		private TList FPersonList;
		private TGenEngine.TShieldState FShieldState;
		private StringList FSourceList;


		public TGEDCOMIndividualRecord Ancestor
		{
			get { return this.FAncestor; }
			set { this.FAncestor = value; }
		}

		public TPedigreeKind Kind
		{
			get { return this.FKind; }
			set { this.FKind = value; }
		}

		public TGenEngine.TShieldState ShieldState
		{
			get { return this.FShieldState; }
			set { this.FShieldState = value; }
		}

		private TPersonObj FindPerson(TGEDCOMIndividualRecord iRec)
		{
			TPersonObj res = null;

			int num = this.FPersonList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if ((this.FPersonList[i] as TPersonObj).iRec == iRec)
				{
					res = (this.FPersonList[i] as TPersonObj);
					break;
				}
			}

			return res;
		}

		private void WritePerson(StreamWriter aStream, TGEDCOMTree aTree, TPersonObj aPerson)
		{
			aStream.WriteLine("<li>");
			aStream.WriteLine("<b>" + this.GetIdStr(aPerson) + ". " + aPerson.iRec.aux_GetNameStr(true, false) + "</b>" + this.GetPedigreeLifeStr(aPerson.iRec));

			if (this.FOptions.PedigreeOptions.IncludeSources)
			{
				aStream.WriteLine("&nbsp;<sup>" + aPerson.Sources + "</sup>");
			}
			PedigreeOptions.TPedigreeFormat format = this.FOptions.PedigreeOptions.Format;
			if (format != PedigreeOptions.TPedigreeFormat.pfExcess)
			{
				if (format == PedigreeOptions.TPedigreeFormat.pfCompact)
				{
					this.WriteCompactFmt(aStream, aPerson);
				}
			}
			else
			{
				this.WriteExcessFmt(aStream, aPerson);
			}
			aStream.WriteLine("</li><br>");
		}

		private string idLink(TPersonObj aObj)
		{
			string res = "";
			if (aObj != null)
			{
				res = " [<a href=\"#" + aObj.Id + "\">" + aObj.Id + "</a>]";
			}
			return res;
		}

		private string GetIdStr(TPersonObj aPerson)
		{
			string Result = "<a name=\"" + aPerson.Id + "\">" + aPerson.Id + "</a>";

			if (this.FKind == TPedigreeKind.pk_Konovalov && aPerson.Parent != null)
			{
				TGEDCOMFamilyRecord family = aPerson.iRec.ChildToFamilyLinks[0].Family;
				string sp_str = "";
				int idx = aPerson.Parent.iRec.IndexOfSpouse(family);
				if (aPerson.Parent.iRec.SpouseToFamilyLinks.Count > 1)
				{
					sp_str = "/" + (idx + 1).ToString();
				}
				Result += sp_str;
			}
			return Result;
		}

		private void WriteExcessFmt(StreamWriter aStream, TPersonObj aPerson)
		{
			aStream.WriteLine("<br>" + LangMan.LSList[87] + ": " + TGenEngine.SexStr(aPerson.iRec.Sex));
			string st = TGenEngine.GetLifeExpectancy(aPerson.iRec);

			if (st != "?" && st != "")
			{
				aStream.WriteLine("<br>" + LangMan.LSList[306] + ": " + st);
			}

			TGEDCOMIndividualRecord father, mother;
			aPerson.iRec.aux_GetParents(out father, out mother);
			if (father != null) aStream.WriteLine("<br>" + LangMan.LSList[150] + ": " + father.aux_GetNameStr(true, false) + this.idLink(this.FindPerson(father)));
			if (mother != null) aStream.WriteLine("<br>" + LangMan.LSList[151] + ": " + mother.aux_GetNameStr(true, false) + this.idLink(this.FindPerson(mother)));

			TList ev_list = new TList(true);
			try
			{
				int i;
				if (aPerson.iRec.IndividualEvents.Count > 0)
				{
					aStream.WriteLine("<p>" + LangMan.LSList[83] + ": <ul>");

					int num = aPerson.iRec.IndividualEvents.Count - 1;
					for (i = 0; i <= num; i++)
					{
						TGEDCOMCustomEvent evt = aPerson.iRec.IndividualEvents[i];
						if (!(evt is TGEDCOMIndividualAttribute) || (evt is TGEDCOMIndividualAttribute && this.FOptions.PedigreeOptions.IncludeAttributes))
						{
							ev_list.Add(new TEventObj(evt, aPerson.iRec));
						}
					}
					this.WriteEventList(aStream, aPerson, ev_list);
					aStream.WriteLine("</ul></p>");
				}

				int num2 = aPerson.iRec.SpouseToFamilyLinks.Count - 1;
				for (i = 0; i <= num2; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.iRec.SpouseToFamilyLinks[i].Family;
					if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						TGEDCOMPointer sp;
						string unk;
						if (aPerson.iRec.Sex == TGEDCOMSex.svMale)
						{
							sp = family.Wife;
							st = LangMan.LSList[116] + ": ";
							unk = LangMan.LSList[63];
						}
						else
						{
							sp = family.Husband;
							st = LangMan.LSList[115] + ": ";
							unk = LangMan.LSList[64];
						}
						TGEDCOMIndividualRecord irec = sp.Value as TGEDCOMIndividualRecord;
						if (irec != null)
						{
							aStream.WriteLine("<p>" + st + irec.aux_GetNameStr(true, false) + this.GetPedigreeLifeStr(irec) + this.idLink(this.FindPerson(irec)));
						}
						else
						{
							aStream.WriteLine("<p>" + st + unk);
						}
						aStream.WriteLine("<ul>");
						ev_list.Clear();

						int num3 = family.Childrens.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							irec = (family.Childrens[j].Value as TGEDCOMIndividualRecord);
							ev_list.Add(new TEventObj(TGenEngine.GetIndividualEvent(irec, "BIRT"), irec));
						}
						this.WriteEventList(aStream, aPerson, ev_list);
						aStream.WriteLine("</ul></p>");
					}
				}
			}
			finally
			{
				ev_list.Dispose();
			}

			if (this.FOptions.PedigreeOptions.IncludeNotes && aPerson.iRec.Notes.Count != 0)
			{
				aStream.WriteLine("<p>" + LangMan.LSList[54] + ":<ul>");

				int num4 = aPerson.iRec.Notes.Count - 1;
				for (int i = 0; i <= num4; i++)
				{
					TGEDCOMNotes note = aPerson.iRec.Notes[i];
					aStream.WriteLine("<li>" + TGenEngine.ConStrings(note.Notes) + "</li>");
				}
				aStream.WriteLine("</ul></p>");
			}
		}

		private void WriteCompactFmt(StreamWriter aStream, TPersonObj aPerson)
		{
			if (this.FOptions.PedigreeOptions.IncludeNotes && aPerson.iRec.Notes.Count != 0)
			{
				int num = aPerson.iRec.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMNotes note = aPerson.iRec.Notes[i];
					aStream.WriteLine("<p style=\"margin-top:2pt; margin-bottom:2pt\">" + TGenEngine.ConStrings(note.Notes) + "</p>");
				}
			}
			try
			{
				bool sp_index = aPerson.iRec.SpouseToFamilyLinks.Count > 1;

				int num2 = aPerson.iRec.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num2; i++)
				{
					TGEDCOMFamilyRecord family = aPerson.iRec.SpouseToFamilyLinks[i].Family;
					if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						TGEDCOMPointer sp;
						string st;
						string unk;
						if (aPerson.iRec.Sex == TGEDCOMSex.svMale)
						{
							sp = family.Wife;
							st = "Ж";
							unk = LangMan.LSList[63];
						}
						else
						{
							sp = family.Husband;
							st = "М";
							unk = LangMan.LSList[64];
						}
						if (sp_index)
						{
							st += (i + 1).ToString();
						}
						st += " - ";
						TGEDCOMIndividualRecord irec = sp.Value as TGEDCOMIndividualRecord;
						if (irec != null)
						{
							st = st + irec.aux_GetNameStr(true, false) + this.GetPedigreeLifeStr(irec) + this.idLink(this.FindPerson(irec));
						}
						else
						{
							st += unk;
						}
						aStream.WriteLine("<p style=\"margin-top:2pt; margin-bottom:2pt\">" + st + "</p>");
					}
				}
			}
			finally
			{
			}
		}

		private string GetPedigreeLifeStr(TGEDCOMIndividualRecord iRec)
		{
			string res_str = "";
			PedigreeOptions.TPedigreeFormat format = this.FOptions.PedigreeOptions.Format;
			if (format != PedigreeOptions.TPedigreeFormat.pfExcess)
			{
				if (format == PedigreeOptions.TPedigreeFormat.pfCompact)
				{
					string ds = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
					string ps = TGenEngine.GetBirthPlace(iRec);
					if (ps != "")
					{
						if (ds != "")
						{
							ds += ", ";
						}
						ds += ps;
					}
					if (ds != "")
					{
						ds = "*" + ds;
					}
					res_str += ds;
					ds = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
					ps = TGenEngine.GetDeathPlace(iRec);
					if (ps != "")
					{
						if (ds != "")
						{
							ds += ", ";
						}
						ds += ps;
					}
					if (ds != "")
					{
						ds = "+" + ds;
					}
					if (ds != "")
					{
						res_str = res_str + " " + ds;
					}
				}
			}
			else
			{
				string ds = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
				if (ds == "")
				{
					ds = "?";
				}
				res_str += ds;
				ds = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
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
					res_str = res_str + " - " + ds;
				}
			}
			string Result;
			if (res_str == "" || res_str == " ")
			{
				Result = "";
			}
			else
			{
				Result = " (" + res_str + ")";
			}
			return Result;
		}

		private void WriteEventList(StreamWriter aStream, TPersonObj aPerson, TList ev_list)
		{
			int num = ev_list.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = ev_list.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					if ((ev_list[i] as TEventObj).GetDate() > (ev_list[j] as TEventObj).GetDate())
					{
						ev_list.Exchange(i, j);
					}
				}
			}

			int num3 = ev_list.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				TGEDCOMCustomEvent evt = (ev_list[i] as TEventObj).Event;
				if (evt != null && object.Equals((ev_list[i] as TEventObj).iRec, aPerson.iRec))
				{
					if (evt.Name == "BIRT")
					{
						ev_list.Exchange(i, 0);
					}
					else
					{
						if (evt.Name == "DEAT")
						{
							ev_list.Exchange(i, ev_list.Count - 1);
						}
					}
				}
			}

			int num4 = ev_list.Count - 1;
			for (int i = 0; i <= num4; i++)
			{
				TEventObj evObj = ev_list[i] as TEventObj;
				TGEDCOMCustomEvent evt = evObj.Event;
				if (object.Equals(evObj.iRec, aPerson.iRec))
				{
					int ev = TGenEngine.GetPersonEventIndex(evt.Name);
					string st;
					if (ev == 0)
					{
						st = evt.Detail.Classification;
					}
					else
					{
						if (ev > 0)
						{
							st = LangMan.LSList[(int)TGenEngine.PersonEvents[ev].Name - 1];
						}
						else
						{
							st = evt.Name;
						}
					}
					string dt = TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
					aStream.WriteLine("<li>" + dt + ": " + st + ".");
					if (evt.Detail.Place.StringValue != "")
					{
						aStream.WriteLine(" " + LangMan.LSList[204] + ": " + evt.Detail.Place.StringValue + "</li>");
					}
				}
				else
				{
					string dt;
					if (evt == null)
					{
						dt = "?";
					}
					else
					{
						dt = TGenEngine.GEDCOMCustomDateToStr(evt.Detail.Date, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
					}
					string st;
					if (evObj.iRec.Sex == TGEDCOMSex.svMale)
					{
						st = ": Родился ";
					}
					else
					{
						st = ": Родилась ";
					}

					aStream.WriteLine("<li>" + dt + st + evObj.iRec.aux_GetNameStr(true, false) + this.idLink(this.FindPerson(evObj.iRec)) + "</li>");
				}
			}
		}

		public override void Generate()
		{
			if (this.FAncestor == null)
			{
				TGenEngine.ShowError(LangMan.LSList[209]);
			}
			else
			{
				string title = LangMan.LSList[484] + ": " + this.FAncestor.aux_GetNameStr(true, false);
				Directory.CreateDirectory(this.FPath);
				StreamWriter fs_index = new StreamWriter(this.FPath + "pedigree.htm", false, Encoding.GetEncoding(1251));
				base.WriteHTMLHeader(fs_index, title);
				fs_index.WriteLine("<h2>" + title + "</h2>");
				this.FPersonList = new TList(true);
				this.FSourceList = new StringList();
				try
				{
					_Generate_Step(null, this.FAncestor, 1, 1);
					_Generate_ReIndex();
					int cur_level = 0;

					int num = this.FPersonList.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TPersonObj pObj = this.FPersonList[i] as TPersonObj;
						if (cur_level != pObj.Level)
						{
							if (cur_level > 0)
							{
								fs_index.WriteLine("</ul>");
							}
							cur_level = pObj.Level;
							fs_index.WriteLine("<h3>"+LangMan.LSList[399]+" "+RomeNumbers.GetRome(cur_level)+"</h3><ul>");
						}
						this.WritePerson(fs_index, this.FTree, pObj);
					}
					fs_index.WriteLine("</ul>");
					if (this.FSourceList.Count > 0)
					{
						fs_index.WriteLine("<h3>" + LangMan.LSList[56] + "</h3>");

						int num2 = this.FSourceList.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							string sn = (j + 1).ToString();
							fs_index.WriteLine("<p><sup><a name=\"src" + sn + "\">" + sn + "</a></sup>&nbsp;");
							fs_index.WriteLine(this.FSourceList[j] + "</p>");
						}
					}
				}
				finally
				{
					this.FSourceList.Free();
					this.FPersonList.Dispose();
				}
				base.WriteHTMLFooter(fs_index);
				SysUtils.Free(fs_index);
				TGenEngine.LoadExtFile(this.FPath + "pedigree.htm");
			}
		}

		public PedigreeExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}

		private void _Generate_Step(TPersonObj aParent, TGEDCOMIndividualRecord iRec, int aLevel, int aFamilyOrder)
		{
			if (iRec != null)
			{
				TPersonObj res = new TPersonObj();
				res.Parent = aParent;
				res.iRec = iRec;
				res.Level = aLevel;
				res.ChildIdx = 0;
				res.BirthDate = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfYYYY_MM_DD, true);
				res.FamilyOrder = aFamilyOrder;
				this.FPersonList.Add(res);
				string i_sources = "";
				int j;
				if (this.FOptions.PedigreeOptions.IncludeSources)
				{
					int num = iRec.SourceCitations.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMSourceCitation cit = iRec.SourceCitations[i];
						TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
						if (sourceRec != null)
						{
							string src_name = TGenEngine.ConStrings(sourceRec.Title);
							if (src_name == "")
							{
								src_name = sourceRec.FiledByEntry;
							}
							j = this.FSourceList.IndexOf(src_name);
							if (j < 0)
							{
								j = this.FSourceList.Add(src_name);
							}
							if (i_sources != "")
							{
								i_sources += ",";
							}
							string sn = (j + 1).ToString();
							i_sources = i_sources + "<a href=\"#src" + sn + "\">" + sn + "</a>";
						}
					}
				}
				res.Sources = i_sources;

				int num2 = iRec.SpouseToFamilyLinks.Count - 1;
				for (j = 0; j <= num2; j++)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
					if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
					{
						family.SortChilds();

						int num3 = family.Childrens.Count - 1;
						for (int i = 0; i <= num3; i++)
						{
							TGEDCOMIndividualRecord child = family.Childrens[i].Value as TGEDCOMIndividualRecord;
							_Generate_Step(res, child, aLevel + 1, i + 1);
						}
					}
				}
			}
		}

		private void _Generate_ReIndex()
		{
			int num = this.FPersonList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = this.FPersonList.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					TPersonObj obj = this.FPersonList[i] as TPersonObj;
					TPersonObj obj2 = this.FPersonList[j] as TPersonObj;
					string i_str = (char)obj.Level + obj.GetOrderStr();
					string k_str = (char)obj2.Level + obj2.GetOrderStr();
					if (string.Compare(i_str, k_str, false) > 0)
					{
						this.FPersonList.Exchange(i, j);
					}
				}
			}

			int num3 = this.FPersonList.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				TPersonObj obj = this.FPersonList[i] as TPersonObj;
				TPedigreeKind fKind = this.FKind;
				if (fKind != TPedigreeKind.pk_dAboville)
				{
					if (fKind == TPedigreeKind.pk_Konovalov)
					{
						obj.Id = (i + 1).ToString();
						if (obj.Parent != null)
						{
							string pid = obj.Parent.Id;

							int p = pid.IndexOf("-");
							if (p >= 0) pid = pid.Substring(0, p);

							obj.Id = obj.Id + "-" + pid;
						}
					}
				}
				else
				{
					if (obj.Parent == null)
					{
						obj.Id = "1";
					}
					else
					{
						obj.Parent.ChildIdx++;
						obj.Id = obj.Parent.Id + "." + obj.Parent.ChildIdx.ToString();
					}
				}
			}
		}
	}
}
