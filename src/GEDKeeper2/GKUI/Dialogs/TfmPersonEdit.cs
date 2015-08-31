using System;
using System.Drawing;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmPersonEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;

        private readonly GKEventsSheet fEventsList;
        private readonly GKSpousesSheet fSpousesList;
        private readonly GKAssociationsSheet fAssociationsList;
        private readonly GKGroupsSheet fGroupsList;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
		private readonly GKSourcesSheet fSourcesList;
        private readonly GKUserRefsSheet fUserRefList;

        private GEDCOMIndividualRecord fPerson;

        public GEDCOMIndividualRecord Person
		{
			get { return this.fPerson; }
			set { this.SetPerson(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}


		private void RefreshPortrait()
		{
			Image img = this.fBase.GetPrimaryBitmap(this.fPerson, this.imgPortrait.Width, this.imgPortrait.Height, false);

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
			if (this.fPerson.PersonalNames.Count > 0)
			{
				GEDCOMPersonalName np = this.fPerson.PersonalNames[0];
				this.edPiecePrefix.Text = np.Pieces.Prefix;
				this.edPieceNickname.Text = np.Pieces.Nickname;
				this.edPieceSurnamePrefix.Text = np.Pieces.SurnamePrefix;
				this.edPieceSuffix.Text = np.Pieces.Suffix;
			}

			if (this.fPerson.ChildToFamilyLinks.Count != 0)
			{
				GEDCOMFamilyRecord family = this.fPerson.ChildToFamilyLinks[0].Family;
				this.btnParentsAdd.Enabled = false;
				this.btnParentsEdit.Enabled = true;
				this.btnParentsDelete.Enabled = true;

                GEDCOMIndividualRecord relPerson = family.Husband.Value as GEDCOMIndividualRecord;
				if (relPerson != null)
				{
					this.btnFatherAdd.Enabled = false;
					this.btnFatherDelete.Enabled = true;
					this.btnFatherSel.Enabled = true;
					this.EditFather.Text = relPerson.aux_GetNameStr(true, false);
				}
				else
				{
					this.btnFatherAdd.Enabled = true;
					this.btnFatherDelete.Enabled = false;
					this.btnFatherSel.Enabled = false;
					this.EditFather.Text = "";
				}

				relPerson = (family.Wife.Value as GEDCOMIndividualRecord);
				if (relPerson != null)
				{
					this.btnMotherAdd.Enabled = false;
					this.btnMotherDelete.Enabled = true;
					this.btnMotherSel.Enabled = true;
					this.EditMother.Text = relPerson.aux_GetNameStr(true, false);
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

		    this.fEventsList.DataList = this.fPerson.IndividualEvents.GetEnumerator();
            this.fNotesList.DataList = this.fPerson.Notes.GetEnumerator();
		    this.fMediaList.DataList = this.fPerson.MultimediaLinks.GetEnumerator();
		    this.fSourcesList.DataList = this.fPerson.SourceCitations.GetEnumerator();
            this.fSpousesList.DataList = this.fPerson.SpouseToFamilyLinks.GetEnumerator();
		    this.fAssociationsList.DataList = this.fPerson.Associations.GetEnumerator();
		    this.fGroupsList.DataList = this.fPerson.Groups.GetEnumerator();
		    this.fUserRefList.DataList = this.fPerson.UserReferences.GetEnumerator();

			this.RefreshPortrait();

			LockEditor(this.fPerson.Restriction == GEDCOMRestriction.rnLocked);
		}

		private void SetPerson(GEDCOMIndividualRecord value)
		{
			this.fPerson = value;
			try
			{
				string fam, nam, pat;
				this.fPerson.GetNameParts(out fam, out nam, out pat);
				this.EditFamily.Text = fam;
				this.EditName.Text = nam;
				this.EditPatronymic.Text = pat;
				this.EditSex.SelectedIndex = (sbyte)this.fPerson.Sex;
				this.CheckPatriarch.Checked = this.fPerson.Patriarch;
				this.chkBookmark.Checked = this.fPerson.Bookmark;
				this.cbRestriction.SelectedIndex = (sbyte)this.fPerson.Restriction;
				this.ControlsRefresh();
			}
			catch (Exception ex)
			{
                this.fBase.Host.LogWrite("TfmPersonEdit.SetPerson(): " + ex.Message);
			}
		}

		private void AcceptChanges()
		{
			GEDCOMPersonalName np = this.fPerson.PersonalNames[0];
			np.SetNameParts(this.EditName.Text.Trim() + " " + this.EditPatronymic.Text.Trim(), this.EditFamily.Text.Trim(), np.LastPart);

			GEDCOMPersonalNamePieces pieces = np.Pieces;
			pieces.Nickname = this.edPieceNickname.Text;
			pieces.Prefix = this.edPiecePrefix.Text;
			pieces.SurnamePrefix = this.edPieceSurnamePrefix.Text;
			pieces.Suffix = this.edPieceSuffix.Text;

			this.fPerson.Sex = (GEDCOMSex)this.EditSex.SelectedIndex;
			this.fPerson.Patriarch = this.CheckPatriarch.Checked;
			this.fPerson.Bookmark = this.chkBookmark.Checked;
			this.fPerson.Restriction = (GEDCOMRestriction)this.cbRestriction.SelectedIndex;

			if (this.fPerson.ChildToFamilyLinks.Count > 0)
			{
				this.fPerson.ChildToFamilyLinks[0].Family.aux_SortChilds();
			}

			this.fBase.ChangeRecord(this.fPerson);
		}

		private void SetTitle()
		{
			this.Text = LangMan.LS(LSID.LSID_Person) + " \"" + this.EditFamily.Text + " " + this.EditName.Text +
				" " + this.EditPatronymic.Text + "\" [" + this.fPerson.GetXRefNum() + "]";
		}

        private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            if (sender == this.fSpousesList && eArgs.Action == RecordAction.raJump)
			{
                GEDCOMFamilyRecord family = eArgs.ItemData as GEDCOMFamilyRecord;
                if (family != null && (this.fPerson.Sex == GEDCOMSex.svMale || this.fPerson.Sex == GEDCOMSex.svFemale))
                {
                    GEDCOMPointer sp = null;
                    switch (this.fPerson.Sex) {
                        case GEDCOMSex.svMale:
                            sp = family.Wife;
                            break;

                        case GEDCOMSex.svFemale:
                            sp = family.Husband;
                            break;
                    }

                    GEDCOMIndividualRecord spouse = sp.Value as GEDCOMIndividualRecord;
                    this.AcceptChanges();
                    this.fBase.SelectRecordByXRef(spouse.XRef);
                    base.Close();
                }
            }
			else
			{
                if (sender == this.fAssociationsList && eArgs.Action == RecordAction.raJump)
				{
                    GEDCOMAssociation ast = eArgs.ItemData as GEDCOMAssociation;

                    this.AcceptChanges();
                    this.fBase.SelectRecordByXRef(ast.Individual.XRef);
                    base.Close();
                }
				else
				{
                    if (sender == this.fGroupsList && eArgs.Action == RecordAction.raJump)
					{
                        GEDCOMGroupRecord grp = eArgs.ItemData as GEDCOMGroupRecord;

                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(grp.XRef);
                        base.Close();
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
			GEDCOMIndividualRecord father = this.fBase.SelectPerson(this.fPerson, TargetMode.tmChild, GEDCOMSex.svMale);
		    if (father == null) return;
		    
            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, true, father);
		    if (family.Husband.Value == null)
		    {
		        family.aux_AddSpouse(father);
		    }
		    this.ControlsRefresh();
		}

		private void btnFatherDelete_Click(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachFatherQuery)) != DialogResult.No)
			{
				GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
				if (family != null)
				{
					family.aux_RemoveSpouse(family.Husband.Value as GEDCOMIndividualRecord);
					this.ControlsRefresh();
				}
			}
		}

		private void btnFatherSel_Click(object sender, EventArgs e)
		{
			GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
		    if (family == null) return;
		    
            this.AcceptChanges();
		    GEDCOMIndividualRecord father = family.Husband.Value as GEDCOMIndividualRecord;
		    this.fBase.SelectRecordByXRef(father.XRef);
		    base.Close();
		}

		private void btnMotherAdd_Click(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord mother = this.fBase.SelectPerson(this.fPerson, TargetMode.tmChild, GEDCOMSex.svFemale);
		    if (mother == null) return;
		    
            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, true, mother);
		    if (family.Wife.Value == null)
		    {
		        family.aux_AddSpouse(mother);
		    }
		    this.ControlsRefresh();
		}

		private void btnMotherDelete_Click(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMotherQuery)) != DialogResult.No)
			{
				GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
				if (family != null)
				{
					GEDCOMIndividualRecord mother = family.Wife.Value as GEDCOMIndividualRecord;
					family.aux_RemoveSpouse(mother);
					this.ControlsRefresh();
				}
			}
		}

		private void btnMotherSel_Click(object sender, EventArgs e)
		{
			GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
		    if (family == null) return;

            this.AcceptChanges();
		    GEDCOMIndividualRecord mother = family.Wife.Value as GEDCOMIndividualRecord;
		    this.fBase.SelectRecordByXRef(mother.XRef);
		    base.Close();
		}

		private void btnParentsAdd_Click(object sender, EventArgs e)
		{
			GEDCOMFamilyRecord family = this.fBase.SelectFamily(this.fPerson);
		    if (family == null) return;

            if (family.IndexOfChild(this.fPerson) < 0)
		    {
		        family.aux_AddChild(this.fPerson);
		    }
		    this.ControlsRefresh();
		}

		private void btnParentsEdit_Click(object sender, EventArgs e)
		{
			GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
			if (family != null && this.fBase.ModifyFamily(ref family, FamilyTarget.ftNone, null))
			{
				this.ControlsRefresh();
			}
		}

		private void btnParentsDelete_Click(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachParentsQuery)) != DialogResult.No)
			{
				GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
				if (family != null)
				{
					family.aux_RemoveChild(this.fPerson);
					this.ControlsRefresh();
				}
			}
		}

		private void btnNameCopy1_Click(object sender, EventArgs e)
		{
			Clipboard.SetDataObject(this.fPerson.aux_GetNameStr(true, false));
		}

		private void btnPortraitAdd_Click(object sender, EventArgs e)
		{
			GEDCOMMultimediaRecord mmRec = fBase.SelectRecord(GEDCOMRecordType.rtMultimedia, null) as GEDCOMMultimediaRecord;
		    if (mmRec == null) return;
		    
            GEDCOMMultimediaLink mmLink = this.fPerson.aux_GetPrimaryMultimediaLink();
		    if (mmLink != null)
		    {
		        mmLink.IsPrimary = false;
		    }
		    this.fPerson.aux_SetPrimaryMultimediaLink(mmRec);
		    this.fMediaList.UpdateSheet();
		    this.RefreshPortrait();
		}

		private void btnPortraitDelete_Click(object sender, EventArgs e)
		{
			GEDCOMMultimediaLink mmLink = this.fPerson.aux_GetPrimaryMultimediaLink();
		    if (mmLink == null) return;
		    
            mmLink.IsPrimary = false;
		    this.RefreshPortrait();
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
			catch (Exception ex)
			{
                this.fBase.Host.LogWrite("TfmPersonEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
		{
			// FIXME: здесь надо бы блокировать интерфейс, а не обновлять его
			// this.ControlsRefresh();
		}

		public TfmPersonEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnPrivacy; res++)
			{
				this.cbRestriction.Items.Add(GKData.Restrictions[(int)res]);
			}

			for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(GKUtils.SexStr(sx));
			}

            this.fEventsList = new GKEventsSheet(this, this.SheetEvents, true);

            this.fSpousesList = new GKSpousesSheet(this, this.SheetSpouses);
			this.fSpousesList.OnModify += this.ListModify;

            this.fAssociationsList = new GKAssociationsSheet(this, this.SheetAssociations);
			this.fAssociationsList.OnModify += this.ListModify;

            this.fGroupsList = new GKGroupsSheet(this, this.SheetGroups);
			this.fGroupsList.OnModify += this.ListModify;

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);
			this.fSourcesList = new GKSourcesSheet(this, this.SheetSources);
            this.fUserRefList = new GKUserRefsSheet(this, this.SheetUserRefs);

			this.btnPortraitAdd.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnPortraitAdd.ImageIndex = 3;
			this.btnPortraitDelete.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnPortraitDelete.ImageIndex = 5;
			this.btnFatherAdd.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnFatherAdd.ImageIndex = 3;
			this.btnFatherDelete.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnFatherDelete.ImageIndex = 5;
			this.btnFatherSel.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnFatherSel.ImageIndex = 28;
			this.btnMotherAdd.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnMotherAdd.ImageIndex = 3;
			this.btnMotherDelete.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnMotherDelete.ImageIndex = 5;
			this.btnMotherSel.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnMotherSel.ImageIndex = 28;
			this.btnParentsAdd.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnParentsAdd.ImageIndex = 3;
			this.btnParentsEdit.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnParentsEdit.ImageIndex = 4;
			this.btnParentsDelete.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
			this.btnParentsDelete.ImageIndex = 5;
			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinPersonEdit);
			this.Label1.Text = LangMan.LS(LSID.LSID_Surname);
			this.Label2.Text = LangMan.LS(LSID.LSID_Name);
			this.Label3.Text = LangMan.LS(LSID.LSID_Patronymic);
			this.Label4.Text = LangMan.LS(LSID.LSID_Sex);
			this.Label7.Text = LangMan.LS(LSID.LSID_Nickname);
			this.Label8.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
			this.Label6.Text = LangMan.LS(LSID.LSID_NamePrefix);
			this.Label9.Text = LangMan.LS(LSID.LSID_NameSuffix);
			this.CheckPatriarch.Text = LangMan.LS(LSID.LSID_Patriarch);
			this.chkBookmark.Text = LangMan.LS(LSID.LSID_Bookmark);
			this.Label12.Text = LangMan.LS(LSID.LSID_Parents);
			this.SheetEvents.Text = LangMan.LS(LSID.LSID_Events);
			this.SheetSpouses.Text = LangMan.LS(LSID.LSID_Spouses);
			this.SheetAssociations.Text = LangMan.LS(LSID.LSID_Associations);
			this.SheetGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
			this.SheetSources.Text = LangMan.LS(LSID.LSID_RPSources);
			this.SheetUserRefs.Text = LangMan.LS(LSID.LSID_UserRefs);
			this.Label5.Text = LangMan.LS(LSID.LSID_Restriction);
		}

        // FIXME
		private void LockEditor(bool locked)
		{
		}
	}
}
