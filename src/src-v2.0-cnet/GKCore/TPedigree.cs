using GedCom551;
using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace GKCore
{

	public class TPedigree : THTMLExporter
	{

		private class TPersonObj
		{

			public TPedigree.TPersonObj Parent;
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
				string Result;
				if (this.Parent == null)
				{
					Result = BDSSystem.WStrFromWChar(order);
				}
				else
				{
					Result = this.Parent.GetOrderStr() + order;
				}
				return Result;
			}

			public void Free()
			{
				TObjectHelper.Free(this);
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
				DateTime Result;
				if (this.Event != null)
				{
					Result = TGenEngine.GEDCOMDateToDate(this.Event.Detail.Date.Value);
				}
				else
				{
					Result = new DateTime((long)((ulong)0));
				}
				return Result;
			}
			public void Free()
			{
				TObjectHelper.Free(this);
			}

		}

		public enum TPedigreeKind : byte
		{
			pk_dAboville,
			pk_Konovalov
		}

		internal TGEDCOMIndividualRecord FAncestor;
		private TPedigree.TPedigreeKind FKind;
		internal TObjectList FPersonList;
		internal TGenEngine.TShieldState FShieldState;
		internal TStringList FSourceList;

		[Browsable(false)]
		public TGEDCOMIndividualRecord Ancestor
		{
			get
			{
				return this.FAncestor;
			}
			set
			{
				this.FAncestor = value;
			}
		}

		[Browsable(false)]
		public TPedigree.TPedigreeKind Kind
		{
			get
			{
				return this.FKind;
			}
			set
			{
				this.FKind = value;
			}
		}

		[Browsable(false)]
		public TGenEngine.TShieldState ShieldState
		{
			get
			{
				return this.FShieldState;
			}
			set
			{
				this.FShieldState = value;
			}
		}

		private TPedigree.TPersonObj FindPerson(TGEDCOMIndividualRecord iRec)
		{
			TPedigree.TPersonObj Result = null;
			int arg_11_0 = 0;
			int num = this.FPersonList.Count - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				while (!object.Equals((this.FPersonList[i] as TPedigree.TPersonObj).iRec, iRec))
				{
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = (this.FPersonList[i] as TPedigree.TPersonObj);
			}
			return Result;
		}

		private void WritePerson(StreamWriter aStream, TGEDCOMTree aTree, TPedigree.TPersonObj aPerson)
		{
			aStream.WriteLine("<li>");
			aStream.WriteLine(string.Concat(new string[]
			{
				"<b>", 
				this.GetIdStr(aPerson), 
				". ", 
				TGenEngine.GetNameStr(aPerson.iRec, true, false), 
				"</b>", 
				this.GetPedigreeLifeStr(aPerson.iRec)
			}));
			if (this.FOptions.PedigreeOptions.IncludeSources)
			{
				aStream.WriteLine("&nbsp;<sup>" + aPerson.Sources + "</sup>");
			}
			TPedigreeOptions.TPedigreeFormat format = this.FOptions.PedigreeOptions.Format;
			if (format != TPedigreeOptions.TPedigreeFormat.pfExcess)
			{
				if (format == TPedigreeOptions.TPedigreeFormat.pfCompact)
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

		private string idLink(TPedigree.TPersonObj aObj)
		{
			string Result = "";
			if (aObj != null)
			{
				Result = string.Concat(new string[]
				{
					" [<a href=\"#", 
					aObj.Id, 
					"\">", 
					aObj.Id, 
					"</a>]"
				});
			}
			return Result;
		}

		private string GetIdStr(TPedigree.TPersonObj aPerson)
		{
			string Result = string.Concat(new string[]
			{
				"<a name=\"", 
				aPerson.Id, 
				"\">", 
				aPerson.Id, 
				"</a>"
			});
			if (this.FKind == TPedigree.TPedigreeKind.pk_Konovalov && aPerson.Parent != null)
			{
				TGEDCOMFamilyRecord family = aPerson.iRec.GetChildToFamilyLink(0).Family;
				string sp_str = "";
				int idx = aPerson.Parent.iRec.IndexOfSpouse(family);
				if (aPerson.Parent.iRec.SpouseToFamilyLinksCount > 1)
				{
					sp_str = "/" + (idx + 1).ToString();
				}
				Result += sp_str;
			}
			return Result;
		}

		private void WriteExcessFmt(StreamWriter aStream, TPedigree.TPersonObj aPerson)
		{
			aStream.WriteLine("<br>" + GKL.LSList[87] + ": " + TGenEngine.SexStr(aPerson.iRec.Sex));
			string st = TGenEngine.GetLifeExpectancy(aPerson.iRec);
			if (BDSSystem.WStrCmp(st, "?") != 0 && BDSSystem.WStrCmp(st, "") != 0)
			{
				aStream.WriteLine("<br>" + GKL.LSList[306] + ": " + st);
			}
			if (aPerson.iRec.ChildToFamilyLinksCount != 0)
			{
				TGEDCOMFamilyRecord family = aPerson.iRec.GetChildToFamilyLink(0).Family;
				TGEDCOMIndividualRecord irec = family.Husband.Value as TGEDCOMIndividualRecord;
				if (irec != null)
				{
					aStream.WriteLine(string.Concat(new string[]
					{
						"<br>", 
						GKL.LSList[150], 
						": ", 
						TGenEngine.GetNameStr(irec, true, false), 
						this.idLink(this.FindPerson(irec))
					}));
				}
				irec = (family.Wife.Value as TGEDCOMIndividualRecord);
				if (irec != null)
				{
					aStream.WriteLine(string.Concat(new string[]
					{
						"<br>", 
						GKL.LSList[151], 
						": ", 
						TGenEngine.GetNameStr(irec, true, false), 
						this.idLink(this.FindPerson(irec))
					}));
				}
			}
			TObjectList ev_list = new TObjectList(true);
			try
			{
				int i;
				if (aPerson.iRec.IndividualEventsCount > 0)
				{
					aStream.WriteLine("<p>" + GKL.LSList[83] + ": <ul>");
					int arg_1B0_0 = 0;
					int num = aPerson.iRec.IndividualEventsCount - 1;
					i = arg_1B0_0;
					if (num >= i)
					{
						num++;
						do
						{
							TGEDCOMCustomEvent @event = aPerson.iRec.GetIndividualEvent(i);
							if (!(@event is TGEDCOMIndividualAttribute) || (@event is TGEDCOMIndividualAttribute && this.FOptions.PedigreeOptions.IncludeAttributes))
							{
								ev_list.Add(new TPedigree.TEventObj(@event, aPerson.iRec));
							}
							i++;
						}
						while (i != num);
					}
					this.WriteEventList(aStream, aPerson, ev_list);
					aStream.WriteLine("</ul></p>");
				}
				int arg_237_0 = 0;
				int num2 = aPerson.iRec.SpouseToFamilyLinksCount - 1;
				i = arg_237_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.iRec.GetSpouseToFamilyLink(i).Family;
						if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
						{
							TGEDCOMPointer sp;
							string unk;
							if (aPerson.iRec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
							{
								sp = family.Wife;
								st = GKL.LSList[116] + ": ";
								unk = GKL.LSList[63];
							}
							else
							{
								sp = family.Husband;
								st = GKL.LSList[115] + ": ";
								unk = GKL.LSList[64];
							}
							TGEDCOMIndividualRecord irec = sp.Value as TGEDCOMIndividualRecord;
							if (irec != null)
							{
								aStream.WriteLine(string.Concat(new string[]
								{
									"<p>", 
									st, 
									TGenEngine.GetNameStr(irec, true, false), 
									this.GetPedigreeLifeStr(irec), 
									this.idLink(this.FindPerson(irec))
								}));
							}
							else
							{
								aStream.WriteLine("<p>" + st + unk);
							}
							aStream.WriteLine("<ul>");
							ev_list.Clear();
							int arg_35C_0 = 0;
							int num3 = family.ChildrenCount - 1;
							int j = arg_35C_0;
							if (num3 >= j)
							{
								num3++;
								do
								{
									irec = (family.GetChildren(j).Value as TGEDCOMIndividualRecord);
									ev_list.Add(new TPedigree.TEventObj(TGenEngine.GetIndividualEvent(irec, "BIRT"), irec));
									j++;
								}
								while (j != num3);
							}
							this.WriteEventList(aStream, aPerson, ev_list);
							aStream.WriteLine("</ul></p>");
						}
						i++;
					}
					while (i != num2);
				}
			}
			finally
			{
				ev_list.Free();
			}
			if (this.FOptions.PedigreeOptions.IncludeNotes && aPerson.iRec.GetNotesCount() != 0)
			{
				aStream.WriteLine("<p>" + GKL.LSList[54] + ":<ul>");
				int arg_41D_0 = 0;
				int num4 = aPerson.iRec.GetNotesCount() - 1;
				int i = arg_41D_0;
				if (num4 >= i)
				{
					num4++;
					do
					{
						TGEDCOMNotes note = aPerson.iRec.GetNote(i);
						aStream.WriteLine("<li>" + TGKSys.ConStrings(note.Notes) + "</li>");
						i++;
					}
					while (i != num4);
				}
				aStream.WriteLine("</ul></p>");
			}
		}

		private void WriteCompactFmt(StreamWriter aStream, TPedigree.TPersonObj aPerson)
		{
			if (this.FOptions.PedigreeOptions.IncludeNotes && aPerson.iRec.GetNotesCount() != 0)
			{
				int arg_2F_0 = 0;
				int num = aPerson.iRec.GetNotesCount() - 1;
				int i = arg_2F_0;
				if (num >= i)
				{
					num++;
					do
					{
						TGEDCOMNotes note = aPerson.iRec.GetNote(i);
						aStream.WriteLine("<p style=\"margin-top:2pt; margin-bottom:2pt\">" + TGKSys.ConStrings(note.Notes) + "</p>");
						i++;
					}
					while (i != num);
				}
			}
			try
			{
				bool sp_index = aPerson.iRec.SpouseToFamilyLinksCount > 1;
				int arg_97_0 = 0;
				int num2 = aPerson.iRec.SpouseToFamilyLinksCount - 1;
				int i = arg_97_0;
				if (num2 >= i)
				{
					num2++;
					do
					{
						TGEDCOMFamilyRecord family = aPerson.iRec.GetSpouseToFamilyLink(i).Family;
						if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
						{
							TGEDCOMPointer sp;
							string st;
							string unk;
							if (aPerson.iRec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
							{
								sp = family.Wife;
								st = "Ж";
								unk = GKL.LSList[63];
							}
							else
							{
								sp = family.Husband;
								st = "М";
								unk = GKL.LSList[64];
							}
							if (sp_index)
							{
								st += (i + 1).ToString();
							}
							st += " - ";
							TGEDCOMIndividualRecord irec = sp.Value as TGEDCOMIndividualRecord;
							if (irec != null)
							{
								st = st + TGenEngine.GetNameStr(irec, true, false) + this.GetPedigreeLifeStr(irec) + this.idLink(this.FindPerson(irec));
							}
							else
							{
								st += unk;
							}
							aStream.WriteLine("<p style=\"margin-top:2pt; margin-bottom:2pt\">" + st + "</p>");
						}
						i++;
					}
					while (i != num2);
				}
			}
			finally
			{
			}
		}
		internal string GetPedigreeLifeStr(TGEDCOMIndividualRecord iRec)
		{
			string res_str = "";
			TPedigreeOptions.TPedigreeFormat format = this.FOptions.PedigreeOptions.Format;
			if (format != TPedigreeOptions.TPedigreeFormat.pfExcess)
			{
				if (format == TPedigreeOptions.TPedigreeFormat.pfCompact)
				{
					string ds = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
					string ps = TGenEngine.GetBirthPlace(iRec);
					if (BDSSystem.WStrCmp(ps, "") != 0)
					{
						if (BDSSystem.WStrCmp(ds, "") != 0)
						{
							ds += ", ";
						}
						ds += ps;
					}
					if (BDSSystem.WStrCmp(ds, "") != 0)
					{
						ds = "*" + ds;
					}
					res_str += ds;
					ds = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
					ps = TGenEngine.GetDeathPlace(iRec);
					if (BDSSystem.WStrCmp(ps, "") != 0)
					{
						if (BDSSystem.WStrCmp(ds, "") != 0)
						{
							ds += ", ";
						}
						ds += ps;
					}
					if (BDSSystem.WStrCmp(ds, "") != 0)
					{
						ds = "+" + ds;
					}
					if (BDSSystem.WStrCmp(ds, "") != 0)
					{
						res_str = res_str + " " + ds;
					}
				}
			}
			else
			{
				string ds = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
				if (BDSSystem.WStrCmp(ds, "") == 0)
				{
					ds = "?";
				}
				res_str += ds;
				ds = TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, true);
				if (BDSSystem.WStrCmp(ds, "") == 0)
				{
					TGEDCOMCustomEvent ev = TGenEngine.GetIndividualEvent(iRec, "DEAT");
					if (ev != null)
					{
						ds = "?";
					}
				}
				if (BDSSystem.WStrCmp(ds, "") != 0)
				{
					res_str = res_str + " - " + ds;
				}
			}
			string Result;
			if (BDSSystem.WStrCmp(res_str, "") == 0 || BDSSystem.WStrCmp(res_str, " ") == 0)
			{
				Result = "";
			}
			else
			{
				Result = " (" + res_str + ")";
			}
			return Result;
		}

		private void WriteEventList(StreamWriter aStream, TPedigree.TPersonObj aPerson, TObjectList ev_list)
		{
			int arg_0B_0 = 0;
			int num = ev_list.Count - 1;
			int i = arg_0B_0;
			if (num >= i)
			{
				num++;
				do
				{
					int arg_23_0 = i + 1;
					int num2 = ev_list.Count - 1;
					int j = arg_23_0;
					if (num2 >= j)
					{
						num2++;
						do
						{
							if ((ev_list[i] as TPedigree.TEventObj).GetDate() > (ev_list[j] as TPedigree.TEventObj).GetDate())
							{
								ev_list.Exchange(i, j);
							}
							j++;
						}
						while (j != num2);
					}
					i++;
				}
				while (i != num);
			}
			int arg_79_0 = 0;
			int num3 = ev_list.Count - 1;
			i = arg_79_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TGEDCOMCustomEvent @event = (ev_list[i] as TPedigree.TEventObj).Event;
					if (@event != null && object.Equals((ev_list[i] as TPedigree.TEventObj).iRec, aPerson.iRec))
					{
						if (BDSSystem.WStrCmp(@event.Name, "BIRT") == 0)
						{
							ev_list.Exchange(i, 0);
						}
						else
						{
							if (BDSSystem.WStrCmp(@event.Name, "DEAT") == 0)
							{
								ev_list.Exchange(i, ev_list.Count - 1);
							}
						}
					}
					i++;
				}
				while (i != num3);
			}
			int arg_109_0 = 0;
			int num4 = ev_list.Count - 1;
			i = arg_109_0;
			if (num4 >= i)
			{
				num4++;
				do
				{
					TPedigree.TEventObj evObj = ev_list[i] as TPedigree.TEventObj;
					TGEDCOMCustomEvent @event = evObj.Event;
					if (object.Equals(evObj.iRec, aPerson.iRec))
					{
						int ev = TGenEngine.GetPersonEventIndex(@event.Name);
						string st;
						if (ev == 0)
						{
							st = @event.Detail.Classification;
						}
						else
						{
							if (ev > 0)
							{
								st = GKL.LSList[(int)TGenEngine.PersonEvents[ev].Name - 1];
							}
							else
							{
								st = @event.Name;
							}
						}
						string dt = TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
						aStream.WriteLine(string.Concat(new string[]
						{
							"<li>", 
							dt, 
							": ", 
							st, 
							"."
						}));
						if (BDSSystem.WStrCmp(@event.Detail.Place.StringValue, "") != 0)
						{
							aStream.WriteLine(string.Concat(new string[]
							{
								" ", 
								GKL.LSList[204], 
								": ", 
								@event.Detail.Place.StringValue, 
								"</li>"
							}));
						}
					}
					else
					{
						string dt;
						if (@event == null)
						{
							dt = "?";
						}
						else
						{
							dt = TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date.Value, TGenEngine.TDateFormat.dfDD_MM_YYYY, false);
						}
						string st;
						if (evObj.iRec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
						{
							st = ": Родился ";
						}
						else
						{
							st = ": Родилась ";
						}
						aStream.WriteLine(string.Concat(new string[]
						{
							"<li>", 
							dt, 
							st, 
							TGenEngine.GetNameStr(evObj.iRec, true, false), 
							this.idLink(this.FindPerson(evObj.iRec)), 
							"</li>"
						}));
					}
					i++;
				}
				while (i != num4);
			}
		}
		public override void Generate()
		{
			if (this.FAncestor == null)
			{
				TGKSys.ShowError(GKL.LSList[209]);
			}
			else
			{
				string title = GKL.LSList[484] + ": " + TGenEngine.GetNameStr(this.FAncestor, true, false);
				VCLUtils.CreateDir(this.FPath);
				StreamWriter fs_index = new StreamWriter(this.FPath + "pedigree.htm", false, Encoding.GetEncoding(1251));
				base.WriteHeader(fs_index, title);
				fs_index.WriteLine("<h2>" + title + "</h2>");
				this.FPersonList = new TObjectList(true);
				this.FSourceList = new TStringList();
				try
				{
					TPedigree._Generate_Step(this, null, this.FAncestor, 1, 1);
					TPedigree._Generate_ReIndex(this);
					int cur_level = 0;
					int arg_D5_0 = 0;
					int num = this.FPersonList.Count - 1;
					int i = arg_D5_0;
					if (num >= i)
					{
						num++;
						do
						{
							TPedigree.TPersonObj pObj = this.FPersonList[i] as TPedigree.TPersonObj;
							if (cur_level != pObj.Level)
							{
								if (cur_level > 0)
								{
									fs_index.WriteLine("</ul>");
								}
								cur_level = pObj.Level;
								fs_index.WriteLine(string.Concat(new string[]
								{
									"<h3>", 
									GKL.LSList[399], 
									" ", 
									TGKSys.GetRome(cur_level), 
									"</h3><ul>"
								}));
							}
							this.WritePerson(fs_index, this.FTree, pObj);
							i++;
						}
						while (i != num);
					}
					fs_index.WriteLine("</ul>");
					if (this.FSourceList.Count > 0)
					{
						fs_index.WriteLine("<h3>" + GKL.LSList[56] + "</h3>");
						int arg_1BF_0 = 0;
						int num2 = this.FSourceList.Count - 1;
						int j = arg_1BF_0;
						if (num2 >= j)
						{
							num2++;
							do
							{
								string sn = (j + 1).ToString();
								fs_index.WriteLine(string.Concat(new string[]
								{
									"<p><sup><a name=\"src", 
									sn, 
									"\">", 
									sn, 
									"</a></sup>&nbsp;"
								}));
								fs_index.WriteLine(this.FSourceList[j] + "</p>");
								j++;
							}
							while (j != num2);
						}
					}
				}
				finally
				{
					this.FSourceList.Free();
					this.FPersonList.Free();
				}
				base.WriteFooter(fs_index);
				TObjectHelper.Free(fs_index);
				TGKSys.LoadExtFile(this.FPath + "pedigree.htm");
			}
		}

		public TPedigree(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}

		private static void _Generate_Step([In] TPedigree Self, TPedigree.TPersonObj aParent, TGEDCOMIndividualRecord iRec, int aLevel, int aFamilyOrder)
		{
			if (iRec != null)
			{
				TPedigree.TPersonObj res = new TPedigree.TPersonObj();
				res.Parent = aParent;
				res.iRec = iRec;
				res.Level = aLevel;
				res.ChildIdx = 0;
				res.BirthDate = TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfYYYY_MM_DD, true);
				res.FamilyOrder = aFamilyOrder;
				Self.FPersonList.Add(res);
				string i_sources = "";
				int j;
				if (Self.FOptions.PedigreeOptions.IncludeSources)
				{
					int arg_7F_0 = 0;
					int num = iRec.GetSourceCitationsCount() - 1;
					int i = arg_7F_0;
					if (num >= i)
					{
						num++;
						do
						{
							TGEDCOMSourceCitation cit = iRec.GetSourceCitation(i);
							TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
							if (sourceRec != null)
							{
								string src_name = TGKSys.ConStrings(sourceRec.Title);
								if (BDSSystem.WStrCmp(src_name, "") == 0)
								{
									src_name = sourceRec.FiledByEntry;
								}
								j = Self.FSourceList.IndexOf(src_name);
								if (j < 0)
								{
									j = Self.FSourceList.Add(src_name);
								}
								if (BDSSystem.WStrCmp(i_sources, "") != 0)
								{
									i_sources += ",";
								}
								string sn = (j + 1).ToString();
								i_sources = string.Concat(new string[]
								{
									i_sources, 
									"<a href=\"#src", 
									sn, 
									"\">", 
									sn, 
									"</a>"
								});
							}
							i++;
						}
						while (i != num);
					}
				}
				res.Sources = i_sources;
				int arg_178_0 = 0;
				int num2 = iRec.SpouseToFamilyLinksCount - 1;
				j = arg_178_0;
				if (num2 >= j)
				{
					num2++;
					do
					{
						TGEDCOMFamilyRecord family = iRec.GetSpouseToFamilyLink(j).Family;
						if (TGenEngine.IsRecordAccess(family.Restriction, Self.FShieldState))
						{
							family.SortChilds();
							int arg_1BC_0 = 0;
							int num3 = family.ChildrenCount - 1;
							int i = arg_1BC_0;
							if (num3 >= i)
							{
								num3++;
								do
								{
									TGEDCOMIndividualRecord child = family.GetChildren(i).Value as TGEDCOMIndividualRecord;
									TPedigree._Generate_Step(Self, res, child, aLevel + 1, i + 1);
									i++;
								}
								while (i != num3);
							}
						}
						j++;
					}
					while (j != num2);
				}
			}
		}
		private static void _Generate_ReIndex([In] TPedigree Self)
		{
			int arg_10_0 = 0;
			int num = Self.FPersonList.Count - 1;
			int i = arg_10_0;
			if (num >= i)
			{
				num++;
				do
				{
					int arg_30_0 = i + 1;
					int num2 = Self.FPersonList.Count - 1;
					int j = arg_30_0;
					if (num2 >= j)
					{
						num2++;
						do
						{
							TPedigree.TPersonObj obj = Self.FPersonList[i] as TPedigree.TPersonObj;
							TPedigree.TPersonObj obj2 = Self.FPersonList[j] as TPedigree.TPersonObj;
							string i_str = (char)obj.Level + obj.GetOrderStr();
							string k_str = (char)obj2.Level + obj2.GetOrderStr();
							if (BDSSystem.WStrCmp(i_str, k_str) > 0)
							{
								Self.FPersonList.Exchange(i, j);
							}
							j++;
						}
						while (j != num2);
					}
					i++;
				}
				while (i != num);
			}
			int arg_CF_0 = 0;
			int num3 = Self.FPersonList.Count - 1;
			i = arg_CF_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TPedigree.TPersonObj obj = Self.FPersonList[i] as TPedigree.TPersonObj;
					TPedigree.TPedigreeKind fKind = Self.FKind;
					if (fKind != TPedigree.TPedigreeKind.pk_dAboville)
					{
						if (fKind == TPedigree.TPedigreeKind.pk_Konovalov)
						{
							obj.Id = (i + 1).ToString();
							if (obj.Parent != null)
							{
								string pid = obj.Parent.Id;
								int p = BDSSystem.Pos("-", pid);
								if (p > 0)
								{
									pid = BDSSystem.WStrCopy(pid, 1, p - 1);
								}
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
					i++;
				}
				while (i != num3);
			}
		}
	}
}
