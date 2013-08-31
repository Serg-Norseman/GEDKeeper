using System;
using System.Drawing;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmPersonEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMIndividualRecord FPerson;
		private GKSheetList FEventsList;
		private GKSheetList FSpousesList;
		private GKSheetList FAssociationsList;
		private GKSheetList FGroupsList;
		private GKSheetList FNotesList;
		private GKSheetList FMediaList;
		private GKSheetList FSourcesList;
		private GKSheetList FUserRefList;
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
					this.EditFather.Text = rel_person.aux_GetNameStr(true, false);
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
					this.EditMother.Text = rel_person.aux_GetNameStr(true, false);
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
						rel_name = rel_person.aux_GetNameStr(true, false);
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

			LockEditor(this.FPerson.Restriction == TGEDCOMRestriction.rnLocked);

			this.PortraitRefresh();
		}

		private void SetPerson(TGEDCOMIndividualRecord Value)
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
			if (pieces.Nickname != this.edPieceNickname.Text)
			{
				pieces.Nickname = this.edPieceNickname.Text;
			}
			if (pieces.Prefix != this.edPiecePrefix.Text)
			{
				pieces.Prefix = this.edPiecePrefix.Text;
			}
			if (pieces.SurnamePrefix != this.edPieceSurnamePrefix.Text)
			{
				pieces.SurnamePrefix = this.edPieceSurnamePrefix.Text;
			}
			if (pieces.Suffix != this.edPieceSuffix.Text)
			{
				pieces.Suffix = this.edPieceSuffix.Text;
			}

			this.FPerson.Sex = (TGEDCOMSex)this.EditSex.SelectedIndex;
			this.FPerson.Patriarch = this.CheckPatriarch.Checked;
			this.FPerson.Bookmark = this.chkBookmark.Checked;
			this.FPerson.Restriction = (TGEDCOMRestriction)this.cbRestriction.SelectedIndex;

			if (this.FPerson.ChildToFamilyLinks.Count > 0)
			{
				this.FPerson.ChildToFamilyLinks[0].Family.aux_SortChilds();
			}

			this.Base.ChangeRecord(this.FPerson);
		}

		private void SetTitle()
		{
			this.Text = LangMan.LSList[96] + " \"" + this.EditFamily.Text + " " + this.EditName.Text +
				" " + this.EditPatronymic.Text + "\" [" + this.FPerson.aux_GetXRefNum() + "]";
		}

		private void ListModify(object sender, object itemData, TRecAction Action)
		{
            bool res = false;

            if (sender == this.FEventsList)
			{
                if (Action == TRecAction.raMoveUp || Action == TRecAction.raMoveDown)
				{
					TGEDCOMCustomEvent evt = itemData as TGEDCOMCustomEvent;
					int idx = this.FPerson.IndividualEvents.IndexOfObject(evt);
					int newIdx = idx;

                    switch (Action)
                    {
                        case TRecAction.raMoveUp:
						    this.FPerson.IndividualEvents.Exchange(idx - 1, idx);
						    newIdx = idx - 1;
                            break;

                        case TRecAction.raMoveDown:
							this.FPerson.IndividualEvents.Exchange(idx, idx + 1);
							newIdx = idx + 1;
                            break;
                    }

                    res = true;
					this.FEventsList.List.SelectItem(newIdx);
				}
				else
				{
                    res = (this.Base.ModifyRecEvent(this, this.FPerson, itemData as TGEDCOMCustomEvent, Action));
				}
			}
			else
			{
				if (sender == this.FSpousesList)
				{
                    TGEDCOMFamilyRecord family = (Action == TRecAction.raAdd) ? null : itemData as TGEDCOMFamilyRecord;

					switch (Action) {
						case TRecAction.raAdd:
                            res = (this.Base.ModifyFamily(ref family, TFamilyTarget.ftSpouse, this.FPerson));
							break;

                        case TRecAction.raEdit:
                            res = (this.Base.ModifyFamily(ref family, TFamilyTarget.ftNone, null));
							break;

                        case TRecAction.raDelete:
							if (family != null && TGenEngine.ShowQuestion(LangMan.LSList[220]) != DialogResult.No)
							{
								family.aux_RemoveSpouse(this.FPerson);
                                res = true;
							}
							break;

                        case TRecAction.raJump:
							if (family != null && (this.FPerson.Sex == TGEDCOMSex.svMale || this.FPerson.Sex == TGEDCOMSex.svFemale)) {
								TGEDCOMPointer sp = null;

								switch (this.FPerson.Sex) {
									case TGEDCOMSex.svMale:
										sp = family.Wife;
										break;

									case TGEDCOMSex.svFemale:
										sp = family.Husband;
										break;
								}

								TGEDCOMIndividualRecord spouse = sp.Value as TGEDCOMIndividualRecord;
								this.AcceptChanges();
								this.Base.SelectRecordByXRef(spouse.XRef);
								base.Close();
							}
							break;

						case TRecAction.raMoveUp:
						case TRecAction.raMoveDown:
						{
							int idx = this.FPerson.IndexOfSpouse(family);
							int newIdx = idx;

                            switch (Action)
                            {
                                case TRecAction.raMoveUp:
								    this.FPerson.ExchangeSpouses(idx - 1, idx);
								    newIdx = idx - 1;
                                    break;

                                case TRecAction.raMoveDown:
									this.FPerson.ExchangeSpouses(idx, idx + 1);
									newIdx = idx + 1;
                                    break;
                            }

                            res = true;
							this.FSpousesList.List.SelectItem(newIdx);
							break;
						}
					}
				}
				else
				{
					if (sender == this.FAssociationsList)
					{
                        res = (this.Base.ModifyRecAssociation(this, this.FPerson, itemData as TGEDCOMAssociation, Action));
					}
					else
					{
						if (sender == this.FGroupsList)
						{
                            res = this.Base.ModifyRecGroup(this, this.FPerson, itemData as TGEDCOMGroupRecord, Action);
						}
						else
						{
							if (sender == this.FNotesList)
							{
                                res = (this.Base.ModifyRecNote(this, this.FPerson, itemData as TGEDCOMNotes, Action));
							}
							else
							{
								if (sender == this.FMediaList)
								{
                                    if (Action == TRecAction.raMoveUp || Action == TRecAction.raMoveDown)
									{
										TGEDCOMMultimediaLink mmLink = itemData as TGEDCOMMultimediaLink;
										int idx = this.FPerson.MultimediaLinks.IndexOfObject(mmLink);
										int newIdx = idx;

                                        switch (Action)
                                        {
                                            case TRecAction.raMoveUp:
											    this.FPerson.MultimediaLinks.Exchange(idx - 1, idx);
											    newIdx = idx - 1;
                                                break;

                                            case TRecAction.raMoveDown:
												this.FPerson.MultimediaLinks.Exchange(idx, idx + 1);
												newIdx = idx + 1;
                                                break;
                                        }

                                        res = true;
										this.FMediaList.List.SelectItem(newIdx);
									} else {
                                        res = (this.Base.ModifyRecMultimedia(this, this.FPerson, itemData as TGEDCOMMultimediaLink, Action));
									}
								}
								else
								{
									if (sender == this.FSourcesList)
									{
                                        if (Action == TRecAction.raMoveUp || Action == TRecAction.raMoveDown)
										{
											TGEDCOMSourceCitation src_cit = itemData as TGEDCOMSourceCitation;
											int idx = this.FPerson.SourceCitations.IndexOfObject(src_cit);
											int newIdx = idx;

                                            switch (Action)
                                            {
                                                case TRecAction.raMoveUp:
												    this.FPerson.SourceCitations.Exchange(idx - 1, idx);
												    newIdx = idx - 1;
                                                    break;

                                                case TRecAction.raMoveDown:
													this.FPerson.SourceCitations.Exchange(idx, idx + 1);
													newIdx = idx + 1;
                                                    break;
                                            }

                                            res = true;
											this.FSourcesList.List.SelectItem(newIdx);
										} else {
                                            res = (this.Base.ModifyRecSource(this, this.FPerson, itemData as TGEDCOMSourceCitation, Action));
										}
									} else {
                                        res = ((sender == this.FUserRefList) && this.Base.ModifyRecUserRef(this, this.FPerson, itemData as TGEDCOMUserReference, Action));
									}
								}
							}
						}
					}
				}
			}

            if (res) this.ControlsRefresh();
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
			TGEDCOMIndividualRecord father = this.Base.SelectPerson(this.FPerson, TTargetMode.tmChild, TGEDCOMSex.svMale);
			if (father != null)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, true, father);
				if (family.Husband.Value == null)
				{
					family.aux_AddSpouse(father);
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
					family.aux_RemoveSpouse(family.Husband.Value as TGEDCOMIndividualRecord);
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
			TGEDCOMIndividualRecord mother = this.Base.SelectPerson(this.FPerson, TTargetMode.tmChild, TGEDCOMSex.svFemale);
			if (mother != null)
			{
				TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, true, mother);
				if (family.Wife.Value == null)
				{
					family.aux_AddSpouse(mother);
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
					family.aux_RemoveSpouse(mother);
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
					family.aux_AddChild(this.FPerson);
				}
				this.ControlsRefresh();
			}
		}

		private void btnParentsEdit_Click(object sender, EventArgs e)
		{
			TGEDCOMFamilyRecord family = this.Base.GetChildFamily(this.FPerson, false, null);
			if (family != null && this.Base.ModifyFamily(ref family, TFamilyTarget.ftNone, null))
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
					family.aux_RemoveChild(this.FPerson);
					this.ControlsRefresh();
				}
			}
		}

		private void btnNameCopy1_Click(object sender, EventArgs e)
		{
			Clipboard.SetDataObject(this.FPerson.aux_GetNameStr(true, false));
		}

		private void btnPortraitAdd_Click(object sender, EventArgs e)
		{
			TGEDCOMMultimediaRecord mmRec = FBase.SelectRecord(TGEDCOMRecordType.rtMultimedia, null) as TGEDCOMMultimediaRecord;
			if (mmRec != null)
			{
				TGEDCOMMultimediaLink mmLink = this.FPerson.aux_GetPrimaryMultimediaLink();
				if (mmLink != null)
				{
					mmLink.IsPrimary = false;
				}
				this.FPerson.aux_SetPrimaryMultimediaLink(mmRec);
				this.PortraitRefresh();
			}
		}

		private void btnPortraitDelete_Click(object sender, EventArgs e)
		{
			TGEDCOMMultimediaLink mmLink = this.FPerson.aux_GetPrimaryMultimediaLink();
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
			// FIXME: здесь надо бы блокировать интерфейс, а не обновлять его
			// this.ControlsRefresh();
		}

		public TfmPersonEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TGEDCOMRestriction res = TGEDCOMRestriction.rnNone; res <= TGEDCOMRestriction.rnPrivacy; res++)
			{
				this.cbRestriction.Items.Add(GKData.Restrictions[(int)res]);
			}

			for (TGEDCOMSex sx = TGEDCOMSex.svNone; sx <= TGEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(TGenEngine.SexStr(sx));
			}

			this.FEventsList = new GKSheetList(this.SheetEvents);
			this.FEventsList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FEventsList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbMoveUp, 
				GKSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecEventsList(this.FEventsList, true);

			this.FSpousesList = new GKSheetList(this.SheetSpouses);
			this.FSpousesList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FSpousesList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump, 
				GKSheetList.TListButton.lbMoveUp, 
				GKSheetList.TListButton.lbMoveDown
			});
			this.FSpousesList.List.AddListColumn("№", 25, false);
			this.FSpousesList.List.AddListColumn(LangMan.LSList[216], 300, false);
			this.FSpousesList.List.AddListColumn(LangMan.LSList[217], 100, false);

			this.FAssociationsList = new GKSheetList(this.SheetAssociations);
			this.FAssociationsList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FAssociationsList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FAssociationsList.List.AddListColumn(LangMan.LSList[95], 300, false);
			this.FAssociationsList.List.AddListColumn(LangMan.LSList[96], 200, false);

			this.FGroupsList = new GKSheetList(this.SheetGroups);
			this.FGroupsList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FGroupsList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbDelete
			});
			this.FGroupsList.List.AddListColumn(LangMan.LSList[185], 350, false);

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new GKSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FMediaList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbMoveUp, 
				GKSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FSourcesList = new GKSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FSourcesList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbMoveUp, 
				GKSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecSourcesList(this.FSourcesList);

			this.FUserRefList = new GKSheetList(this.SheetUserRefs);
			this.FUserRefList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
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

		private void LockEditor(bool aLocked)
		{
		}
	}
}
