using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKSys;
using GKUI.Lists;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI
{
	public partial class TfmPersonEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMIndividualRecord FPerson;
		private TSheetList FEventsList;
		private TSheetList FSpousesList;
		private TSheetList FAssociationsList;
		private TSheetList FGroupsList;
		private TSheetList FNotesList;
		private TSheetList FMediaList;
		private TSheetList FSourcesList;
		private TSheetList FUserRefList;
		private TextBox EditMother;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMIndividualRecord Person
		{
			get { return this.FPerson; }
			set { this.SetPerson(value); }
		}

		private void PortraitRefresh()
		{
			Image img = this.Base.Engine.GetPrimaryBitmap(this.FPerson, this.imgPortrait.Width, this.imgPortrait.Height, false);

			if (img != null)
			{
				this.imgPortrait.Image = img; // освобождать нельзя, изображение исчезает
				this.imgPortrait.SizeMode = PictureBoxSizeMode.CenterImage;

				this.imgPortrait.Visible = true;
			}
			else
			{
				this.imgPortrait.Visible = false;
			}
		}

		private void ControlsRefresh()
		{
			if (this.FPerson.PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = this.FPerson.PersonalNames[0];
				this.edPiecePrefix.Text = np.Pieces.Prefix;
				this.edPieceNickname.Text = np.Pieces.Nickname;
				this.edPieceSurnamePrefix.Text = np.Pieces.SurnamePrefix;
				this.edPieceSuffix.Text = np.Pieces.Suffix;
			}
			if (this.FPerson.ChildToFamilyLinks.Count != 0)
			{
				TGEDCOMFamilyRecord family = this.FPerson.ChildToFamilyLinks[0].Family;
				this.btnParentsAdd.Enabled = false;
				this.btnParentsEdit.Enabled = true;
				this.btnParentsDelete.Enabled = true;
				TGEDCOMIndividualRecord rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
				if (rel_person != null)
				{
					this.btnFatherAdd.Enabled = false;
					this.btnFatherDelete.Enabled = true;
					this.btnFatherSel.Enabled = true;
					this.EditFather.Text = TGenEngine.GetNameStr(rel_person, true, false);
				}
				else
				{
					this.btnFatherAdd.Enabled = true;
					this.btnFatherDelete.Enabled = false;
					this.btnFatherSel.Enabled = false;
					this.EditFather.Text = "";
				}
				rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
				if (rel_person != null)
				{
					this.btnMotherAdd.Enabled = false;
					this.btnMotherDelete.Enabled = true;
					this.btnMotherSel.Enabled = true;
					this.EditMother.Text = TGenEngine.GetNameStr(rel_person, true, false);
				}
				else
				{
					this.btnMotherAdd.Enabled = true;
					this.btnMotherDelete.Enabled = false;
					this.btnMotherSel.Enabled = false;
					this.EditMother.Text = "";
				}
			}
			else
			{
				this.btnParentsAdd.Enabled = true;
				this.btnParentsEdit.Enabled = false;
				this.btnParentsDelete.Enabled = false;
				this.btnFatherAdd.Enabled = true;
				this.btnFatherDelete.Enabled = false;
				this.btnFatherSel.Enabled = false;
				this.btnMotherAdd.Enabled = true;
				this.btnMotherDelete.Enabled = false;
				this.btnMotherSel.Enabled = false;
				this.EditFather.Text = "";
				this.EditMother.Text = "";
			}
			this.Base.RecListIndividualEventsRefresh(this.FPerson, this.FEventsList.List, null);
			this.Base.RecListNotesRefresh(this.FPerson, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FPerson, this.FMediaList.List, null);
			this.Base.RecListSourcesRefresh(this.FPerson, this.FSourcesList.List, null);
			this.FSpousesList.List.Items.Clear();

			int num = this.FPerson.SpouseToFamilyLinks.Count;
			for (int idx = 1; idx <= num; idx++)
			{
				TGEDCOMFamilyRecord family = this.FPerson.SpouseToFamilyLinks[idx - 1].Family;
				if (family != null)
				{
					TGEDCOMIndividualRecord rel_person;
					string rel_name;
					if (this.FPerson.Sex == TGEDCOMSex.svMale)
					{
						rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
						rel_name = LangMan.LSList[63];
					}
					else
					{
						rel_person = (family.Husband.Value as TGEDCOMIndividualRecord);
						rel_name = LangMan.LSList[64];
					}
					if (rel_person != null)
					{
						rel_name = TGenEngine.GetNameStr(rel_person, true, false);
					}
					ListViewItem item = this.FSpousesList.List.AddItem(idx.ToString(), family);
					item.SubItems.Add(rel_name);
					item.SubItems.Add(TGenEngine.GetMarriageDate(family, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
				}
			}
			this.Base.RecListAssociationsRefresh(this.FPerson, this.FAssociationsList.List, null);
			this.Base.RecListGroupsRefresh(this.FPerson, this.FGroupsList.List, null);

			this.FUserRefList.List.Items.Clear();
			int num2 = this.FPerson.UserReferences.Count - 1;
			for (int idx = 0; idx <= num2; idx++)
			{
				TGEDCOMUserReference uref = this.FPerson.UserReferences[idx];
				ListViewItem item = this.FUserRefList.List.AddItem(uref.StringValue, uref);
				item.SubItems.Add(uref.ReferenceType);
			}
			TfmPersonEdit._ControlsRefresh_LockEditor(this.FPerson.Restriction == TGEDCOMRestriction.rnLocked);
			this.PortraitRefresh();
		}

		private void SetPerson([In] TGEDCOMIndividualRecord Value)
		{
			this.FPerson = Value;
			try
			{
				string fam, nam, pat;
				this.FPerson.aux_GetNameParts(out fam, out nam, out pat);
				this.EditFamily.Text = fam;
				this.EditName.Text = nam;
				this.EditPatronymic.Text = pat;
				this.EditSex.SelectedIndex = (int)((sbyte)this.FPerson.Sex);
				this.CheckPatriarch.Checked = this.FPerson.Patriarch;
				this.chkBookmark.Checked = this.FPerson.Bookmark;
				this.cbRestriction.SelectedIndex = (int)((sbyte)this.FPerson.Restriction);
				this.ControlsRefresh();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("PersonEdit.SetPerson(): " + E.Message);
			}
		}

		private void AcceptChanges()
		{
			TGEDCOMPersonalName np = this.FPerson.PersonalNames[0];
			np.SetNameParts(this.EditName.Text.Trim() + " " + this.EditPatronymic.Text.Trim(), this.EditFamily.Text.Trim(), np.LastPart);

			TGEDCOMPersonalNamePieces pieces = np.Pieces;
			if (pieces.Prefix != this.edPiecePrefix.Text)
			{
				pieces.Prefix = this.edPiecePrefix.Text;
			}
			if (pieces.Nickname != this.edPieceNickname.Text)
			{
				pieces.Nickname = this.edPieceNickname.Text;
			}
			if (pieces.SurnamePrefix != this.edPieceSurnamePrefix.Text)
			{
				pieces.SurnamePrefix = this.edPieceSurnamePrefix.Text;
			}
			if (pieces.Suffix != this.edPieceSuffix.Text)
			{
				pieces.Suffix = this.edPieceSuffix.Text;
			}

			this.Base.DoPersonChangeSex(this.FPerson, (TGEDCOMSex)this.EditSex.SelectedIndex);
			this.Base.DoPersonChangePatriarch(this.FPerson, this.CheckPatriarch.Checked);

			this.FPerson.Bookmark = this.chkBookmark.Checked;
			this.FPerson.Restriction = (TGEDCOMRestriction)this.cbRestriction.SelectedIndex;

			if (this.FPerson.ChildToFamilyLinks.Count > 0)
			{
				this.FPerson.ChildToFamilyLinks[0].Family.SortChilds();
			}

			this.Base.ChangeRecord(this.FPerson);
		}

		private void SetTitle()
		{
			this.Text = LangMan.LSList[96] + " \"" + this.EditFamily.Text + " " + this.EditName.Text +
				" " + this.EditPatronymic.Text + "\" [" + TGenEngine.GetId(this.FPerson).ToString() + "]";
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FEventsList))
			{
				if (Action >= TGenEngine.TRecAction.raMoveUp && Action < (TGenEngine.TRecAction)6)
				{
					TGEDCOMCustomEvent @event = ItemData as TGEDCOMCustomEvent;
					int idx = this.FPerson.IndividualEvents.IndexOfObject(@event);
					if (Action != TGenEngine.TRecAction.raMoveUp)
					{
						if (Action == TGenEngine.TRecAction.raMoveDown)
						{
							this.FPerson.IndividualEvents.Exchange(idx, idx + 1);
						}
					}
					else
					{
						this.FPerson.IndividualEvents.Exchange(idx - 1, idx);
					}
					this.ControlsRefresh();
				}
				else
				{
					if (this.Base.ModifyRecEvent(this, this.FPerson, ItemData as TGEDCOMCustomEvent, Action))
					{
						this.ControlsRefresh();
					}
				}
			}
			else
			{
				if (object.Equals(Sender, this.FSpousesList))
				{
					switch (Action)
					{
						case TGenEngine.TRecAction.raAdd:
						{
							TGEDCOMFamilyRecord family = null;
							if (this.Base.ModifyFamily(ref family, TGenEngine.TFamilyTarget.ftSpouse, this.FPerson))
							{
								this.ControlsRefresh();
							}
							break;
						}
						case TGenEngine.TRecAction.raEdit:
						{
							TGEDCOMFamilyRecord family = ItemData as TGEDCOMFamilyRecord;
							if (this.Base.ModifyFamily(ref family, TGenEngine.TFamilyTarget.ftNone, null))
							{
								this.ControlsRefresh();
							}
							break;
						}
						case TGenEngine.TRecAction.raDelete:
						{
							TGEDCOMFamilyRecord family = ItemData as TGEDCOMFamilyRecord;
							if (family != null && TGenEngine.ShowQuestion(LangMan.LSList[220]) != DialogResult.No)
							{
								this.Base.Engine.RemoveFamilySpouse(family, this.FPerson);
								this.ControlsRefresh();
							}
							break;
						}
						case TGenEngine.TRecAction.raJump:
						{
							TGEDCOMFamilyRecord family = ItemData as TGEDCOMFamilyRecord;
							if (family != null)
							{
								TGEDCOMSex sex = this.FPerson.Sex;
								if (sex != TGEDCOMSex.svNone)
								{
									TGEDCOMPointer sp = null;
									if (sex != TGEDCOMSex.svMale)
									{
										if (sex != TGEDCOMSex.svFemale)
										{
											if (sex == TGEDCOMSex.svUndetermined)
											{
												break;
											}
										}
										else
										{
											sp = family.Husband;
										}
									}
									else
									{
										sp = family.Wife;
									}
									TGEDCOMIndividualRecord spouse = sp.Value as TGEDCOMIndividualRecord;
									this.AcceptChanges();
									this.Base.SelectRecordByXRef(spouse.XRef);
									base.Close();
								}
							}
							break;
						}
						case TGenEngine.TRecAction.raMoveUp:
						case TGenEngine.TRecAction.raMoveDown:
						{
							TGEDCOMFamilyRecord family = ItemData as TGEDCOMFamilyRecord;
							int idx = this.FPerson.IndexOfSpouse(family);
							if (Action != TGenEngine.TRecAction.raMoveUp)
							{
								if (Action == TGenEngine.TRecAction.raMoveDown)
								{
									this.FPerson.ExchangeSpouses(idx, idx + 1);
								}
							}
							else
							{
								this.FPerson.ExchangeSpouses(idx - 1, idx);
							}
							this.ControlsRefresh();
							break;
						}
					}
				}
				else
				{
					if (object.Equals(Sender, this.FAssociationsList))
					{
						if (this.Base.ModifyRecAssociation(this, this.FPerson, ItemData as TGEDCOMAssociation, Action))
						{
							this.ControlsRefresh();
						}
					}
					else
					{
						if (object.Equals(Sender, this.FGroupsList))
						{
							if (Action != TGenEngine.TRecAction.raAdd)
							{
								if (Action == TGenEngine.TRecAction.raDelete)
								{
									TGEDCOMGroupRecord group = ItemData as TGEDCOMGroupRecord;
									if (TGenEngine.ShowQuestion(LangMan.LSList[188]) != DialogResult.No && this.Base.Engine.RemoveGroupMember(group, this.FPerson))
									{
										this.ControlsRefresh();
									}
								}
							}
							else
							{
								TfmBase arg_281_0 = this.Base;
								TGEDCOMRecordType arg_281_1 = TGEDCOMRecordType.rtGroup;
								object[] anArgs = new object[0];
								TGEDCOMGroupRecord group = arg_281_0.SelectRecord(arg_281_1, anArgs) as TGEDCOMGroupRecord;
								if (group != null && this.Base.Engine.AddGroupMember(group, this.FPerson))
								{
									this.ControlsRefresh();
								}
							}
						}
						else
						{
							if (object.Equals(Sender, this.FNotesList))
							{
								if (this.Base.ModifyRecNote(this, this.FPerson, ItemData as TGEDCOMNotes, Action))
								{
									this.ControlsRefresh();
								}
							}
							else
							{
								if (object.Equals(Sender, this.FMediaList))
								{
									if (Action >= TGenEngine.TRecAction.raMoveUp && Action < (TGenEngine.TRecAction)6)
									{
										TGEDCOMMultimediaLink mmLink = ItemData as TGEDCOMMultimediaLink;
										int idx = this.FPerson.MultimediaLinks.IndexOfObject(mmLink);
										if (Action != TGenEngine.TRecAction.raMoveUp)
										{
											if (Action == TGenEngine.TRecAction.raMoveDown)
											{
												this.FPerson.MultimediaLinks.Exchange(idx, idx + 1);
											}
										}
										else
										{
											this.FPerson.MultimediaLinks.Exchange(idx - 1, idx);
										}
										this.ControlsRefresh();
									}
									else
									{
										if (this.Base.ModifyRecMultimedia(this, this.FPerson, ItemData as TGEDCOMMultimediaLink, Action))
										{
											this.ControlsRefresh();
										}
									}
								}
								else
								{
									if (object.Equals(Sender, this.FSourcesList))
									{
										if (Action >= TGenEngine.TRecAction.raMoveUp && Action < (TGenEngine.TRecAction)6)
										{
											TGEDCOMSourceCitation src_cit = ItemData as TGEDCOMSourceCitation;
											int idx = this.FPerson.SourceCitations.IndexOfObject(src_cit);
											if (Action != TGenEngine.TRecAction.raMoveUp)
											{
												if (Action == TGenEngine.TRecAction.raMoveDown)
												{
													this.FPerson.SourceCitations.Exchange(idx, idx + 1);
												}
											}
											else
											{
												this.FPerson.SourceCitations.Exchange(idx - 1, idx);
											}
											this.ControlsRefresh();
										}
										else
										{
											if (this.Base.ModifyRecSource(this, this.FPerson, ItemData as TGEDCOMSourceCitation, Action))
											{
												this.ControlsRefresh();
											}
										}
									}
									else
									{
										if (object.Equals(Sender, this.FUserRefList) && this.Base.ModifyRecUserRef(this, this.FPerson, ItemData as TGEDCOMUserReference, Action))
										{
											this.ControlsRefresh();
										}
									}
								}
							}
						}
					}
				}
			}
		}

		private void EditFamily_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}

		private void EditName_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}

		private void EditPatronymic_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}

		private void btnFatherAdd_Click(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord father = this.Base.SelectPerson(this.FPerson, TGenEngine.TTargetMode.tmChild, TGEDCOMSex.svMale);
			if (father != null)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, true, father);
				if (family.Husband.Value == null)
				{
					this.Base.Engine.AddFamilySpouse(family, father);
				}
				this.ControlsRefresh();
			}
		}

		private void btnFatherDelete_Click(object sender, EventArgs e)
		{
			if (TGenEngine.ShowQuestion(LangMan.LSList[218]) != DialogResult.No)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
				if (family != null)
				{
					this.Base.Engine.RemoveFamilySpouse(family, family.Husband.Value as TGEDCOMIndividualRecord);
					this.ControlsRefresh();
				}
			}
		}

		private void btnFatherSel_Click(object sender, EventArgs e)
		{
			TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
			if (family != null)
			{
				this.AcceptChanges();
				TGEDCOMIndividualRecord father = family.Husband.Value as TGEDCOMIndividualRecord;
				this.Base.SelectRecordByXRef(father.XRef);
				base.Close();
			}
		}

		private void btnMotherAdd_Click(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord mother = this.Base.SelectPerson(this.FPerson, TGenEngine.TTargetMode.tmChild, TGEDCOMSex.svFemale);
			if (mother != null)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, true, mother);
				if (family.Wife.Value == null)
				{
					this.Base.Engine.AddFamilySpouse(family, mother);
				}
				this.ControlsRefresh();
			}
		}

		private void btnMotherDelete_Click(object sender, EventArgs e)
		{
			if (TGenEngine.ShowQuestion(LangMan.LSList[219]) != DialogResult.No)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
				if (family != null)
				{
					TGEDCOMIndividualRecord mother = family.Wife.Value as TGEDCOMIndividualRecord;
					this.Base.Engine.RemoveFamilySpouse(family, mother);
					this.ControlsRefresh();
				}
			}
		}

		private void btnMotherSel_Click(object sender, EventArgs e)
		{
			TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
			if (family != null)
			{
				this.AcceptChanges();
				TGEDCOMIndividualRecord mother = family.Wife.Value as TGEDCOMIndividualRecord;
				this.Base.SelectRecordByXRef(mother.XRef);
				base.Close();
			}
		}

		private void btnParentsAdd_Click(object sender, EventArgs e)
		{
			TGEDCOMFamilyRecord family = this.Base.SelectFamily(this.FPerson);
			if (family != null)
			{
				if (family.IndexOfChild(this.FPerson) < 0)
				{
					this.Base.Engine.AddFamilyChild(family, this.FPerson);
				}
				this.ControlsRefresh();
			}
		}

		private void btnParentsEdit_Click(object sender, EventArgs e)
		{
			TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
			if (family != null && this.Base.ModifyFamily(ref family, TGenEngine.TFamilyTarget.ftNone, null))
			{
				this.ControlsRefresh();
			}
		}

		private void btnParentsDelete_Click(object sender, EventArgs e)
		{
			if (TGenEngine.ShowQuestion(LangMan.LSList[221]) != DialogResult.No)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
				if (family != null)
				{
					this.Base.Engine.RemoveFamilyChild(family, this.FPerson);
					this.ControlsRefresh();
				}
			}
		}

		private void btnNameCopy1_Click(object sender, EventArgs e)
		{
			Clipboard.SetDataObject(TGenEngine.GetNameStr(this.FPerson, true, false));
		}

		private void btnPortraitAdd_Click(object sender, EventArgs e)
		{
			TGEDCOMRecordType arg_0F_1 = TGEDCOMRecordType.rtMultimedia;
			object[] anArgs = new object[0];
			TGEDCOMMultimediaRecord mmRec = FBase.SelectRecord(arg_0F_1, anArgs) as TGEDCOMMultimediaRecord;
			if (mmRec != null)
			{
				TGEDCOMMultimediaLink mmLink = this.Base.Engine.GetPrimaryMultimediaLink(this.FPerson);
				if (mmLink != null)
				{
					mmLink.IsPrimary = false;
				}
				this.Base.Engine.SetPrimaryMultimediaRecord(this.FPerson, mmRec);
				this.PortraitRefresh();
			}
		}

		private void btnPortraitDelete_Click(object sender, EventArgs e)
		{
			TGEDCOMMultimediaLink mmLink = this.Base.Engine.GetPrimaryMultimediaLink(this.FPerson);
			if (mmLink != null)
			{
				mmLink.IsPrimary = false;
				this.PortraitRefresh();
			}
		}

		private void EditFamily_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '/')
			{
				e.Handled = true;
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmPersonEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
		{
			//fixme: здесь надо бы блокировать интерфейс, а не обновлять его
			//this.ControlsRefresh();
		}

		public TfmPersonEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TGEDCOMRestriction res = TGEDCOMRestriction.rnNone; res <= TGEDCOMRestriction.rnPrivacy; res++)
			{
				this.cbRestriction.Items.Add(TGenEngine.Restrictions[(int)res]);
			}

			for (TGEDCOMSex sx = TGEDCOMSex.svNone; sx <= TGEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(TGenEngine.SexStr(sx));
			}

			this.FEventsList = new TSheetList(this.SheetEvents);
			this.FEventsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FEventsList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecEventsList(this.FEventsList, true);

			this.FSpousesList = new TSheetList(this.SheetSpouses);
			this.FSpousesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FSpousesList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump, 
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.FSpousesList.List.AddListColumn("№", 25, false);
			this.FSpousesList.List.AddListColumn(LangMan.LSList[216], 300, false);
			this.FSpousesList.List.AddListColumn(LangMan.LSList[217], 100, false);

			this.FAssociationsList = new TSheetList(this.SheetAssociations);
			this.FAssociationsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FAssociationsList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FAssociationsList.List.AddListColumn(LangMan.LSList[95], 300, false);
			this.FAssociationsList.List.AddListColumn(LangMan.LSList[96], 200, false);

			this.FGroupsList = new TSheetList(this.SheetGroups);
			this.FGroupsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FGroupsList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbDelete
			});
			this.FGroupsList.List.AddListColumn(LangMan.LSList[185], 350, false);

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FMediaList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FSourcesList.Buttons.Include(new Enum[]
			{
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecSourcesList(this.FSourcesList);

			this.FUserRefList = new TSheetList(this.SheetUserRefs);
			this.FUserRefList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FUserRefList.List.AddListColumn(LangMan.LSList[112], 300, false);
			this.FUserRefList.List.AddListColumn(LangMan.LSList[113], 200, false);

			this.btnPortraitAdd.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnPortraitAdd.ImageIndex = 3;
			this.btnPortraitDelete.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnPortraitDelete.ImageIndex = 5;
			this.btnFatherAdd.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnFatherAdd.ImageIndex = 3;
			this.btnFatherDelete.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnFatherDelete.ImageIndex = 5;
			this.btnFatherSel.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnFatherSel.ImageIndex = 28;
			this.btnMotherAdd.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnMotherAdd.ImageIndex = 3;
			this.btnMotherDelete.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnMotherDelete.ImageIndex = 5;
			this.btnMotherSel.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnMotherSel.ImageIndex = 28;
			this.btnParentsAdd.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnParentsAdd.ImageIndex = 3;
			this.btnParentsEdit.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnParentsEdit.ImageIndex = 4;
			this.btnParentsDelete.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnParentsDelete.ImageIndex = 5;
			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[103];
			this.Label1.Text = LangMan.LSList[84];
			this.Label2.Text = LangMan.LSList[85];
			this.Label3.Text = LangMan.LSList[86];
			this.Label4.Text = LangMan.LSList[87];
			this.Label7.Text = LangMan.LSList[88];
			this.Label8.Text = LangMan.LSList[89];
			this.Label6.Text = LangMan.LSList[90];
			this.Label9.Text = LangMan.LSList[91];
			this.CheckPatriarch.Text = LangMan.LSList[92];
			this.chkBookmark.Text = LangMan.LSList[93];
			this.Label12.Text = LangMan.LSList[152];
			this.SheetEvents.Text = LangMan.LSList[83];
			this.SheetSpouses.Text = LangMan.LSList[153];
			this.SheetAssociations.Text = LangMan.LSList[154];
			this.SheetGroups.Text = LangMan.LSList[58];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
			this.SheetSources.Text = LangMan.LSList[56];
			this.SheetUserRefs.Text = LangMan.LSList[155];
			this.Label5.Text = LangMan.LSList[124];
		}

		private static void _ControlsRefresh_LockEditor(bool aLocked)
		{
		}
	}
}
