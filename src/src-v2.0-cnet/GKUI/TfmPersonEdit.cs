using GedCom551;
using GKCore;
using GKUI.Lists;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmPersonEdit : Form
	{
		private TabControl PagesPersonData;
		private TabPage SheetEvents;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private TabPage SheetSources;
		private TabPage SheetSpouses;
		private TabPage SheetAssociations;
		private TabPage SheetGroups;
		private Button btnAccept;
		private Button btnCancel;
		private Label Label5;
		private ComboBox cbRestriction;
		private GroupBox GroupBox1;
		private Label Label1;
		private Label Label2;
		private Label Label3;
		private Label Label4;
		private TextBox EditFamily;
		private TextBox EditName;
		private TextBox EditPatronymic;
		private ComboBox EditSex;
		private CheckBox CheckPatriarch;
		private TabPage SheetUserRefs;
		private Panel PageCtlParents;
		private Label Label12;
		private TextBox EditFather;
		private Button btnParentsAdd;
		private Button btnParentsEdit;
		private Button btnParentsDelete;
		private CheckBox chkBookmark;
		private Label Label8;
		private TextBox edPieceSurnamePrefix;
		private Label Label6;
		private TextBox edPiecePrefix;
		private Label Label9;
		private TextBox edPieceSuffix;
		private Label Label7;
		private TextBox edPieceNickname;
		private Panel panPortrait;
		private PictureBox imgPortrait;
		private Button btnNameCopy;
		private Button btnPortraitAdd;
		private Button btnPortraitDelete;
		private Button btnFatherAdd;
		private Button btnFatherDelete;
		private Button btnFatherSel;
		private Button btnMotherAdd;
		private Button btnMotherDelete;
		private Button btnMotherSel;
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
		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMIndividualRecord Person
		{
			get
			{
				return this.FPerson;
			}
			set
			{
				this.SetPerson(value);
			}
		}
		private void PortraitRefresh()
		{
			Bitmap bmp = this.Base.Engine.GetPrimaryBitmap(this.FPerson);
			if (bmp != null)
			{
				this.imgPortrait.Image = bmp;
				if (bmp.Width > this.imgPortrait.Width || bmp.Height > this.imgPortrait.Height)
				{
					this.imgPortrait.SizeMode = PictureBoxSizeMode.StretchImage;
				}
				TObjectHelper.Free(bmp);
				this.imgPortrait.Visible = true;
			}
			else
			{
				this.imgPortrait.Visible = false;
			}
		}
		private void ControlsRefresh()
		{
			if (this.FPerson.PersonalNamesCount > 0)
			{
				TGEDCOMPersonalName np = this.FPerson.GetPersonalName(0);
				this.edPiecePrefix.Text = np.Pieces.Prefix;
				this.edPieceNickname.Text = np.Pieces.Nickname;
				this.edPieceSurnamePrefix.Text = np.Pieces.SurnamePrefix;
				this.edPieceSuffix.Text = np.Pieces.Suffix;
			}
			if (this.FPerson.ChildToFamilyLinksCount != 0)
			{
				TGEDCOMFamilyRecord family = this.FPerson.GetChildToFamilyLink(0).Family;
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
			int arg_2F5_0 = 1;
			int num = this.FPerson.SpouseToFamilyLinksCount;
			int idx = arg_2F5_0;
			if (num >= idx)
			{
				num++;
				do
				{
					TGEDCOMFamilyRecord family = this.FPerson.GetSpouseToFamilyLink(idx - 1).Family;
					if (family != null)
					{
						TGEDCOMIndividualRecord rel_person;
						string rel_name;
						if (this.FPerson.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
						{
							rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
							rel_name = GKL.LSList[63];
						}
						else
						{
							rel_person = (family.Husband.Value as TGEDCOMIndividualRecord);
							rel_name = GKL.LSList[64];
						}
						if (rel_person != null)
						{
							rel_name = TGenEngine.GetNameStr(rel_person, true, false);
						}
						ListViewItem item = this.FSpousesList.List.AddItem(idx.ToString(), family);
						item.SubItems.Add(rel_name);
						item.SubItems.Add(TGenEngine.GetMarriageDate(family, GKL.fmGEDKeeper.Options.DefDateFormat));
					}
					idx++;
				}
				while (idx != num);
			}
			this.Base.RecListAssociationsRefresh(this.FPerson, this.FAssociationsList.List, null);
			this.Base.RecListGroupsRefresh(this.FPerson, this.FGroupsList.List, null);
			this.FUserRefList.List.Items.Clear();
			int arg_434_0 = 0;
			int num2 = this.FPerson.GetUserReferencesCount() - 1;
			idx = arg_434_0;
			if (num2 >= idx)
			{
				num2++;
				do
				{
					TGEDCOMUserReference uref = this.FPerson.GetUserReference(idx);
					ListViewItem item = this.FUserRefList.List.AddItem(uref.StringValue, uref);
					item.SubItems.Add(uref.ReferenceType);
					idx++;
				}
				while (idx != num2);
			}
			TfmPersonEdit._ControlsRefresh_LockEditor(this.FPerson.Restriction == TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.PortraitRefresh();
		}
		private void SetPerson([In] TGEDCOMIndividualRecord Value)
		{
			this.FPerson = Value;
			try
			{
				string fam = "";
				string nam = "";
				string pat = "";
				TGenEngine.GetNameParts(this.FPerson, ref fam, ref nam, ref pat);
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
				TGKSys.LogWrite("PersonEdit.SetPerson(): " + E.Message);
			}
		}
		private void AcceptChanges()
		{
			TGEDCOMPersonalName np = this.FPerson.GetPersonalName(0);
			np.SetNameParts(this.EditName.Text.Trim() + " " + this.EditPatronymic.Text.Trim(), this.EditFamily.Text.Trim(), np.LastPart);
			TGEDCOMPersonalNamePieces pieces = np.Pieces;
			if (BDSSystem.WStrCmp(pieces.Prefix, this.edPiecePrefix.Text) != 0)
			{
				pieces.Prefix = this.edPiecePrefix.Text;
			}
			if (BDSSystem.WStrCmp(pieces.Nickname, this.edPieceNickname.Text) != 0)
			{
				pieces.Nickname = this.edPieceNickname.Text;
			}
			if (BDSSystem.WStrCmp(pieces.SurnamePrefix, this.edPieceSurnamePrefix.Text) != 0)
			{
				pieces.SurnamePrefix = this.edPieceSurnamePrefix.Text;
			}
			if (BDSSystem.WStrCmp(pieces.Suffix, this.edPieceSuffix.Text) != 0)
			{
				pieces.Suffix = this.edPieceSuffix.Text;
			}
			this.Base.DoPersonChangeSex(this.FPerson, (TGEDCOMObject.TGEDCOMSex)this.EditSex.SelectedIndex);
			this.Base.DoPersonChangePatriarch(this.FPerson, this.CheckPatriarch.Checked);
			this.FPerson.Bookmark = this.chkBookmark.Checked;
			this.FPerson.Restriction = (TGEDCOMObject.TGEDCOMRestriction)this.cbRestriction.SelectedIndex;
			if (this.FPerson.ChildToFamilyLinksCount > 0)
			{
				this.FPerson.GetChildToFamilyLink(0).Family.SortChilds();
			}
			this.Base.ChangeRecord(this.FPerson);
		}
		private void SetTitle()
		{
			this.Text = string.Concat(new string[]
			{
				GKL.LSList[96], 
				" \"", 
				this.EditFamily.Text, 
				" ", 
				this.EditName.Text, 
				" ", 
				this.EditPatronymic.Text, 
				"\" [", 
				TGenEngine.GetId(this.FPerson).ToString(), 
				"]"
			});
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FEventsList))
			{
				if (Action >= TGenEngine.TRecAction.raMoveUp && Action < (TGenEngine.TRecAction)6)
				{
					TGEDCOMCustomEvent @event = ItemData as TGEDCOMCustomEvent;
					int idx = this.FPerson.IndexOfEvent(@event);
					if (Action != TGenEngine.TRecAction.raMoveUp)
					{
						if (Action == TGenEngine.TRecAction.raMoveDown)
						{
							this.FPerson.ExchangeEvents(idx, idx + 1);
						}
					}
					else
					{
						this.FPerson.ExchangeEvents(idx - 1, idx);
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
							if (family != null && TGKSys.ShowQuestion(GKL.LSList[220]) != DialogResult.No)
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
								TGEDCOMObject.TGEDCOMSex sex = this.FPerson.Sex;
								if (sex != TGEDCOMObject.TGEDCOMSex.svNone)
								{
									TGEDCOMPointer sp = null;
									if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
									{
										if (sex != TGEDCOMObject.TGEDCOMSex.svFemale)
										{
											if (sex == TGEDCOMObject.TGEDCOMSex.svUndetermined)
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
									if (TGKSys.ShowQuestion(GKL.LSList[188]) != DialogResult.No && this.Base.Engine.RemoveGroupMember(group, this.FPerson))
									{
										this.ControlsRefresh();
									}
								}
							}
							else
							{
								TfmBase arg_281_0 = this.Base;
								TGEDCOMRecord.TGEDCOMRecordType arg_281_1 = TGEDCOMRecord.TGEDCOMRecordType.rtGroup;
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
										int idx = this.FPerson.IndexOfMultimediaLink(mmLink);
										if (Action != TGenEngine.TRecAction.raMoveUp)
										{
											if (Action == TGenEngine.TRecAction.raMoveDown)
											{
												this.FPerson.ExchangeMedia(idx, idx + 1);
											}
										}
										else
										{
											this.FPerson.ExchangeMedia(idx - 1, idx);
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
											int idx = this.FPerson.IndexOfSourceCitation(src_cit);
											if (Action != TGenEngine.TRecAction.raMoveUp)
											{
												if (Action == TGenEngine.TRecAction.raMoveDown)
												{
													this.FPerson.ExchangeSources(idx, idx + 1);
												}
											}
											else
											{
												this.FPerson.ExchangeSources(idx - 1, idx);
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
		private void InitializeComponent()
		{
			this.PagesPersonData = new TabControl();
			this.SheetEvents = new TabPage();
			this.SheetSpouses = new TabPage();
			this.SheetAssociations = new TabPage();
			this.SheetGroups = new TabPage();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.SheetSources = new TabPage();
			this.SheetUserRefs = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.Label5 = new Label();
			this.cbRestriction = new ComboBox();
			this.GroupBox1 = new GroupBox();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.Label4 = new Label();
			this.Label8 = new Label();
			this.Label6 = new Label();
			this.Label9 = new Label();
			this.Label7 = new Label();
			this.btnPortraitAdd = new Button();
			this.btnPortraitDelete = new Button();
			this.EditFamily = new TextBox();
			this.EditName = new TextBox();
			this.EditPatronymic = new TextBox();
			this.EditSex = new ComboBox();
			this.CheckPatriarch = new CheckBox();
			this.PageCtlParents = new Panel();
			this.EditMother = new TextBox();
			this.Label12 = new Label();
			this.btnParentsAdd = new Button();
			this.btnParentsEdit = new Button();
			this.btnParentsDelete = new Button();
			this.btnFatherAdd = new Button();
			this.btnFatherDelete = new Button();
			this.btnFatherSel = new Button();
			this.btnMotherAdd = new Button();
			this.btnMotherDelete = new Button();
			this.btnMotherSel = new Button();
			this.EditFather = new TextBox();
			this.chkBookmark = new CheckBox();
			this.edPieceSurnamePrefix = new TextBox();
			this.edPiecePrefix = new TextBox();
			this.edPieceSuffix = new TextBox();
			this.edPieceNickname = new TextBox();
			this.panPortrait = new Panel();
			this.imgPortrait = new PictureBox();
			this.btnNameCopy = new Button();
			this.PagesPersonData.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			this.PageCtlParents.SuspendLayout();
			this.panPortrait.SuspendLayout();
			base.SuspendLayout();
			this.PagesPersonData.Controls.Add(this.SheetEvents);
			this.PagesPersonData.Controls.Add(this.SheetSpouses);
			this.PagesPersonData.Controls.Add(this.SheetAssociations);
			this.PagesPersonData.Controls.Add(this.SheetGroups);
			this.PagesPersonData.Controls.Add(this.SheetNotes);
			this.PagesPersonData.Controls.Add(this.SheetMultimedia);
			this.PagesPersonData.Controls.Add(this.SheetSources);
			this.PagesPersonData.Controls.Add(this.SheetUserRefs);
			this.PagesPersonData.Location = new Point(0, 265);
			this.PagesPersonData.Name = "PagesPersonData";
			this.PagesPersonData.SelectedIndex = 0;
			this.PagesPersonData.Size = new Size(625, 264);
			this.PagesPersonData.TabIndex = 1;
			this.SheetEvents.Location = new Point(4, 22);
			this.SheetEvents.Name = "SheetEvents";
			this.SheetEvents.Size = new Size(617, 238);
			this.SheetEvents.TabIndex = 0;
			this.SheetEvents.Text = "Факты";
			this.SheetSpouses.Location = new Point(4, 22);
			this.SheetSpouses.Name = "SheetSpouses";
			this.SheetSpouses.Size = new Size(617, 238);
			this.SheetSpouses.TabIndex = 1;
			this.SheetSpouses.Text = "Супруги";
			this.SheetAssociations.Location = new Point(4, 22);
			this.SheetAssociations.Name = "SheetAssociations";
			this.SheetAssociations.Size = new Size(617, 238);
			this.SheetAssociations.TabIndex = 2;
			this.SheetAssociations.Text = "Ассоциации";
			this.SheetGroups.Location = new Point(4, 22);
			this.SheetGroups.Name = "SheetGroups";
			this.SheetGroups.Size = new Size(617, 238);
			this.SheetGroups.TabIndex = 3;
			this.SheetGroups.Text = "Группы";
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(617, 238);
			this.SheetNotes.TabIndex = 4;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(617, 238);
			this.SheetMultimedia.TabIndex = 5;
			this.SheetMultimedia.Text = "Мультимедиа";
			this.SheetSources.Location = new Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new Size(617, 238);
			this.SheetSources.TabIndex = 6;
			this.SheetSources.Text = "Источники";
			this.SheetUserRefs.Location = new Point(4, 22);
			this.SheetUserRefs.Name = "SheetUserRefs";
			this.SheetUserRefs.Size = new Size(617, 238);
			this.SheetUserRefs.TabIndex = 7;
			this.SheetUserRefs.Text = "Сноски/Пометки";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(448, 544);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(536, 544);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.Label5.Location = new Point(8, 552);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(150, 13);
			this.Label5.TabIndex = 0;
			this.Label5.Text = "Ограничение безопасности";
			this.cbRestriction.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbRestriction.Location = new Point(160, 544);
			this.cbRestriction.Name = "cbRestriction";
			this.cbRestriction.Size = new Size(145, 21);
			this.cbRestriction.TabIndex = 2;
			this.cbRestriction.SelectedIndexChanged += new EventHandler(this.cbRestriction_SelectedIndexChanged);
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label3);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label8);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.Label9);
			this.GroupBox1.Controls.Add(this.Label7);
			this.GroupBox1.Controls.Add(this.btnPortraitAdd);
			this.GroupBox1.Controls.Add(this.btnPortraitDelete);
			this.GroupBox1.Controls.Add(this.EditFamily);
			this.GroupBox1.Controls.Add(this.EditName);
			this.GroupBox1.Controls.Add(this.EditPatronymic);
			this.GroupBox1.Controls.Add(this.EditSex);
			this.GroupBox1.Controls.Add(this.CheckPatriarch);
			this.GroupBox1.Controls.Add(this.PageCtlParents);
			this.GroupBox1.Controls.Add(this.chkBookmark);
			this.GroupBox1.Controls.Add(this.edPieceSurnamePrefix);
			this.GroupBox1.Controls.Add(this.edPiecePrefix);
			this.GroupBox1.Controls.Add(this.edPieceSuffix);
			this.GroupBox1.Controls.Add(this.edPieceNickname);
			this.GroupBox1.Controls.Add(this.panPortrait);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(625, 265);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.Label1.Location = new Point(8, 16);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(55, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Фамилия";
			this.Label2.Location = new Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(25, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Имя";
			this.Label3.Location = new Point(8, 96);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(55, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Отчество";
			this.Label4.Location = new Point(184, 136);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(25, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Пол";
			this.Label8.Location = new Point(184, 16);
			this.Label8.Name = "Label8";
			this.Label8.Size = new Size(105, 13);
			this.Label8.TabIndex = 4;
			this.Label8.Text = "Префикс фамилии";
			this.Label6.Location = new Point(184, 56);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(105, 13);
			this.Label6.TabIndex = 5;
			this.Label6.Text = "Префикс имени";
			this.Label9.Location = new Point(184, 96);
			this.Label9.Name = "Label9";
			this.Label9.Size = new Size(105, 13);
			this.Label9.TabIndex = 6;
			this.Label9.Text = "Суффикс имени";
			this.Label7.Location = new Point(8, 136);
			this.Label7.Name = "Label7";
			this.Label7.Size = new Size(60, 13);
			this.Label7.TabIndex = 7;
			this.Label7.Text = "Прозвище";
			this.btnPortraitAdd.AccessibleDescription = "Присоединить портрет";
			this.btnPortraitAdd.Location = new Point(552, 152);
			this.btnPortraitAdd.Name = "btnPortraitAdd";
			this.btnPortraitAdd.Size = new Size(26, 26);
			this.btnPortraitAdd.TabIndex = 8;
			this.btnPortraitAdd.Click += new EventHandler(this.btnPortraitAdd_Click);
			this.btnPortraitDelete.AccessibleDescription = "Отсоединить портрет";
			this.btnPortraitDelete.Location = new Point(584, 152);
			this.btnPortraitDelete.Name = "btnPortraitDelete";
			this.btnPortraitDelete.Size = new Size(26, 26);
			this.btnPortraitDelete.TabIndex = 9;
			this.btnPortraitDelete.Click += new EventHandler(this.btnPortraitDelete_Click);
			this.EditFamily.Location = new Point(8, 32);
			this.EditFamily.Name = "EditFamily";
			this.EditFamily.Size = new Size(161, 21);
			this.EditFamily.TabIndex = 0;
			this.EditFamily.Text = "";
			this.EditFamily.KeyPress += new KeyPressEventHandler(this.EditFamily_KeyPress);
			this.EditFamily.TextChanged += new EventHandler(this.EditFamily_TextChanged);
			this.EditName.Location = new Point(8, 72);
			this.EditName.Name = "EditName";
			this.EditName.Size = new Size(161, 21);
			this.EditName.TabIndex = 1;
			this.EditName.Text = "";
			this.EditName.KeyPress += new KeyPressEventHandler(this.EditFamily_KeyPress);
			this.EditName.TextChanged += new EventHandler(this.EditName_TextChanged);
			this.EditPatronymic.Location = new Point(8, 112);
			this.EditPatronymic.Name = "EditPatronymic";
			this.EditPatronymic.Size = new Size(161, 21);
			this.EditPatronymic.TabIndex = 2;
			this.EditPatronymic.Text = "";
			this.EditPatronymic.KeyPress += new KeyPressEventHandler(this.EditFamily_KeyPress);
			this.EditPatronymic.TextChanged += new EventHandler(this.EditPatronymic_TextChanged);
			this.EditSex.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditSex.Location = new Point(184, 152);
			this.EditSex.Name = "EditSex";
			this.EditSex.TabIndex = 7;
			this.CheckPatriarch.Location = new Point(320, 144);
			this.CheckPatriarch.Name = "CheckPatriarch";
			this.CheckPatriarch.Size = new Size(153, 17);
			this.CheckPatriarch.TabIndex = 8;
			this.CheckPatriarch.Text = "Патриарх (глава семьи)";
			this.PageCtlParents.BorderStyle = BorderStyle.FixedSingle;
			this.PageCtlParents.Controls.Add(this.EditMother);
			this.PageCtlParents.Controls.Add(this.Label12);
			this.PageCtlParents.Controls.Add(this.btnParentsAdd);
			this.PageCtlParents.Controls.Add(this.btnParentsEdit);
			this.PageCtlParents.Controls.Add(this.btnParentsDelete);
			this.PageCtlParents.Controls.Add(this.btnFatherAdd);
			this.PageCtlParents.Controls.Add(this.btnFatherDelete);
			this.PageCtlParents.Controls.Add(this.btnFatherSel);
			this.PageCtlParents.Controls.Add(this.btnMotherAdd);
			this.PageCtlParents.Controls.Add(this.btnMotherDelete);
			this.PageCtlParents.Controls.Add(this.btnMotherSel);
			this.PageCtlParents.Controls.Add(this.EditFather);
			this.PageCtlParents.Location = new Point(2, 192);
			this.PageCtlParents.Name = "PageCtlParents";
			this.PageCtlParents.Size = new Size(621, 71);
			this.PageCtlParents.TabIndex = 10;
			this.EditMother.ForeColor = SystemColors.Control;
			this.EditMother.Location = new Point(296, 8);
			this.EditMother.Name = "EditMother";
			this.EditMother.ReadOnly = true;
			this.EditMother.Size = new Size(224, 21);
			this.EditMother.TabIndex = 10;
			this.EditMother.Text = "";
			this.Label12.Location = new Point(8, 16);
			this.Label12.Name = "Label12";
			this.Label12.Size = new Size(55, 13);
			this.Label12.TabIndex = 0;
			this.Label12.Text = "Родители";
			this.btnParentsAdd.AccessibleDescription = "Присоединить семью родителей";
			this.btnParentsAdd.Location = new Point(525, 5);
			this.btnParentsAdd.Name = "btnParentsAdd";
			this.btnParentsAdd.Size = new Size(26, 26);
			this.btnParentsAdd.TabIndex = 1;
			this.btnParentsAdd.Click += new EventHandler(this.btnParentsAdd_Click);
			this.btnParentsEdit.AccessibleDescription = "Правка семьи родителей";
			this.btnParentsEdit.Location = new Point(554, 5);
			this.btnParentsEdit.Name = "btnParentsEdit";
			this.btnParentsEdit.Size = new Size(26, 26);
			this.btnParentsEdit.TabIndex = 2;
			this.btnParentsEdit.Click += new EventHandler(this.btnParentsEdit_Click);
			this.btnParentsDelete.AccessibleDescription = "Отсоединить семью родителей";
			this.btnParentsDelete.Location = new Point(583, 5);
			this.btnParentsDelete.Name = "btnParentsDelete";
			this.btnParentsDelete.Size = new Size(26, 26);
			this.btnParentsDelete.TabIndex = 3;
			this.btnParentsDelete.Click += new EventHandler(this.btnParentsDelete_Click);
			this.btnFatherAdd.AccessibleDescription = "Выбрать или добавить отца";
			this.btnFatherAdd.Location = new Point(198, 35);
			this.btnFatherAdd.Name = "btnFatherAdd";
			this.btnFatherAdd.Size = new Size(26, 26);
			this.btnFatherAdd.TabIndex = 4;
			this.btnFatherAdd.Click += new EventHandler(this.btnFatherAdd_Click);
			this.btnFatherDelete.AccessibleDescription = "Отсоединить отца";
			this.btnFatherDelete.Location = new Point(230, 35);
			this.btnFatherDelete.Name = "btnFatherDelete";
			this.btnFatherDelete.Size = new Size(26, 26);
			this.btnFatherDelete.TabIndex = 5;
			this.btnFatherDelete.Click += new EventHandler(this.btnFatherDelete_Click);
			this.btnFatherSel.AccessibleDescription = "Перейти на запись отца";
			this.btnFatherSel.Location = new Point(262, 35);
			this.btnFatherSel.Name = "btnFatherSel";
			this.btnFatherSel.Size = new Size(26, 26);
			this.btnFatherSel.TabIndex = 6;
			this.btnFatherSel.Click += new EventHandler(this.btnFatherSel_Click);
			this.btnMotherAdd.AccessibleDescription = "Выбрать или добавить мать";
			this.btnMotherAdd.Location = new Point(430, 35);
			this.btnMotherAdd.Name = "btnMotherAdd";
			this.btnMotherAdd.Size = new Size(26, 26);
			this.btnMotherAdd.TabIndex = 7;
			this.btnMotherAdd.Click += new EventHandler(this.btnMotherAdd_Click);
			this.btnMotherDelete.AccessibleDescription = "Отсоединить мать";
			this.btnMotherDelete.Location = new Point(462, 35);
			this.btnMotherDelete.Name = "btnMotherDelete";
			this.btnMotherDelete.Size = new Size(26, 26);
			this.btnMotherDelete.TabIndex = 8;
			this.btnMotherDelete.Click += new EventHandler(this.btnMotherDelete_Click);
			this.btnMotherSel.AccessibleDescription = "Перейти на запись матери";
			this.btnMotherSel.Location = new Point(494, 35);
			this.btnMotherSel.Name = "btnMotherSel";
			this.btnMotherSel.Size = new Size(26, 26);
			this.btnMotherSel.TabIndex = 9;
			this.btnMotherSel.Click += new EventHandler(this.btnMotherSel_Click);
			this.EditFather.ForeColor = SystemColors.Control;
			this.EditFather.Location = new Point(64, 8);
			this.EditFather.Name = "EditFather";
			this.EditFather.ReadOnly = true;
			this.EditFather.Size = new Size(224, 21);
			this.EditFather.TabIndex = 0;
			this.EditFather.Text = "";
			this.chkBookmark.Location = new Point(320, 161);
			this.chkBookmark.Name = "chkBookmark";
			this.chkBookmark.Size = new Size(153, 17);
			this.chkBookmark.TabIndex = 9;
			this.chkBookmark.Text = "Закладка";
			this.edPieceSurnamePrefix.Location = new Point(184, 32);
			this.edPieceSurnamePrefix.Name = "edPieceSurnamePrefix";
			this.edPieceSurnamePrefix.Size = new Size(121, 21);
			this.edPieceSurnamePrefix.TabIndex = 4;
			this.edPieceSurnamePrefix.Text = "";
			this.edPiecePrefix.Location = new Point(184, 72);
			this.edPiecePrefix.Name = "edPiecePrefix";
			this.edPiecePrefix.Size = new Size(121, 21);
			this.edPiecePrefix.TabIndex = 5;
			this.edPiecePrefix.Text = "";
			this.edPieceSuffix.Location = new Point(184, 112);
			this.edPieceSuffix.Name = "edPieceSuffix";
			this.edPieceSuffix.Size = new Size(121, 21);
			this.edPieceSuffix.TabIndex = 6;
			this.edPieceSuffix.Text = "";
			this.edPieceNickname.Location = new Point(8, 152);
			this.edPieceNickname.Name = "edPieceNickname";
			this.edPieceNickname.Size = new Size(161, 21);
			this.edPieceNickname.TabIndex = 3;
			this.edPieceNickname.Text = "";
			this.panPortrait.Controls.Add(this.imgPortrait);
			this.panPortrait.Location = new Point(480, 16);
			this.panPortrait.Name = "panPortrait";
			this.panPortrait.Size = new Size(130, 130);
			this.panPortrait.TabIndex = 11;
			this.imgPortrait.Location = new Point(1, 1);
			this.imgPortrait.Name = "imgPortrait";
			this.imgPortrait.Size = new Size(128, 128);
			this.imgPortrait.TabIndex = 0;
			this.imgPortrait.TabStop = false;
			this.btnNameCopy.AccessibleDescription = "Скопировать имя в буфер обмена";
			this.btnNameCopy.Location = new Point(384, 544);
			this.btnNameCopy.Name = "btnNameCopy";
			this.btnNameCopy.Size = new Size(33, 25);
			this.btnNameCopy.TabIndex = 1;
			this.btnNameCopy.Click += new EventHandler(this.btnNameCopy1_Click);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(625, 577);
			base.Controls.Add(this.Label5);
			base.Controls.Add(this.btnNameCopy);
			base.Controls.Add(this.PagesPersonData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.cbRestriction);
			base.Controls.Add(this.GroupBox1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmPersonEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Редактирование персональной информации";
			this.PagesPersonData.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			this.PageCtlParents.ResumeLayout(false);
			this.panPortrait.ResumeLayout(false);
			base.ResumeLayout(false);
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
			TGEDCOMIndividualRecord father = this.Base.SelectPerson(this.FPerson, TGenEngine.TTargetMode.tmDescendant, TGEDCOMObject.TGEDCOMSex.svMale);
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
			if (TGKSys.ShowQuestion(GKL.LSList[218]) != DialogResult.No)
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
			TGEDCOMIndividualRecord mother = this.Base.SelectPerson(this.FPerson, TGenEngine.TTargetMode.tmDescendant, TGEDCOMObject.TGEDCOMSex.svFemale);
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
			if (TGKSys.ShowQuestion(GKL.LSList[219]) != DialogResult.No)
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
			if (TGKSys.ShowQuestion(GKL.LSList[221]) != DialogResult.No)
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
			TfmBase arg_0F_0 = this.Base;
			TGEDCOMRecord.TGEDCOMRecordType arg_0F_1 = TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia;
			object[] anArgs = new object[0];
			TGEDCOMMultimediaRecord mmRec = arg_0F_0.SelectRecord(arg_0F_1, anArgs) as TGEDCOMMultimediaRecord;
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
				base.DialogResult = DialogResult.None;
			}
		}
		private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.ControlsRefresh();
		}
		public TfmPersonEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TGEDCOMObject.TGEDCOMRestriction res = TGEDCOMObject.TGEDCOMRestriction.rnNone; res <= TGEDCOMObject.TGEDCOMRestriction.rnPrivacy; res++)
			{
				this.cbRestriction.Items.Add(TGenEngine.Restrictions[(int)res]);
			}

			for (TGEDCOMObject.TGEDCOMSex sx = TGEDCOMObject.TGEDCOMSex.svNone; sx <= TGEDCOMObject.TGEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(TGenEngine.SexStr(sx));
			}

			this.FEventsList = new TSheetList(this.SheetEvents);
			this.FEventsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FEventsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecEventsList(this.FEventsList, true);
			this.FSpousesList = new TSheetList(this.SheetSpouses);
			this.FSpousesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FSpousesList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump, 
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.FSpousesList.List.AddListColumn("№", 25, false);
			this.FSpousesList.List.AddListColumn(GKL.LSList[216], 300, false);
			this.FSpousesList.List.AddListColumn(GKL.LSList[217], 100, false);
			this.FAssociationsList = new TSheetList(this.SheetAssociations);
			this.FAssociationsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FAssociationsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FAssociationsList.List.AddListColumn(GKL.LSList[95], 300, false);
			this.FAssociationsList.List.AddListColumn(GKL.LSList[96], 200, false);
			this.FGroupsList = new TSheetList(this.SheetGroups);
			this.FGroupsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FGroupsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbDelete
			});
			this.FGroupsList.List.AddListColumn(GKL.LSList[185], 350, false);
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FMediaList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecMediaList(this.FMediaList);
			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FSourcesList.Buttons.Include(new Enum[]
			{
				TSheetList.TListButton.lbMoveUp, 
				TSheetList.TListButton.lbMoveDown
			});
			this.Base.SetupRecSourcesList(this.FSourcesList);
			this.FUserRefList = new TSheetList(this.SheetUserRefs);
			this.FUserRefList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FUserRefList.List.AddListColumn(GKL.LSList[112], 300, false);
			this.FUserRefList.List.AddListColumn(GKL.LSList[113], 200, false);
			this.btnPortraitAdd.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnPortraitAdd.ImageIndex = 3;
			this.btnPortraitDelete.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnPortraitDelete.ImageIndex = 5;
			this.btnFatherAdd.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnFatherAdd.ImageIndex = 3;
			this.btnFatherDelete.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnFatherDelete.ImageIndex = 5;
			this.btnFatherSel.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnFatherSel.ImageIndex = 28;
			this.btnMotherAdd.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnMotherAdd.ImageIndex = 3;
			this.btnMotherDelete.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnMotherDelete.ImageIndex = 5;
			this.btnMotherSel.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnMotherSel.ImageIndex = 28;
			this.btnParentsAdd.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnParentsAdd.ImageIndex = 3;
			this.btnParentsEdit.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnParentsEdit.ImageIndex = 4;
			this.btnParentsDelete.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.btnParentsDelete.ImageIndex = 5;
			this.SetLang();
		}
		public void SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[103];
			this.Label1.Text = GKL.LSList[84];
			this.Label2.Text = GKL.LSList[85];
			this.Label3.Text = GKL.LSList[86];
			this.Label4.Text = GKL.LSList[87];
			this.Label7.Text = GKL.LSList[88];
			this.Label8.Text = GKL.LSList[89];
			this.Label6.Text = GKL.LSList[90];
			this.Label9.Text = GKL.LSList[91];
			this.CheckPatriarch.Text = GKL.LSList[92];
			this.chkBookmark.Text = GKL.LSList[93];
			this.Label12.Text = GKL.LSList[152];
			this.SheetEvents.Text = GKL.LSList[83];
			this.SheetSpouses.Text = GKL.LSList[153];
			this.SheetAssociations.Text = GKL.LSList[154];
			this.SheetGroups.Text = GKL.LSList[58];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
			this.SheetSources.Text = GKL.LSList[56];
			this.SheetUserRefs.Text = GKL.LSList[155];
			this.Label5.Text = GKL.LSList[124];
		}

		private static void _ControlsRefresh_LockEditor(bool aLocked)
		{
		}
	}
}
