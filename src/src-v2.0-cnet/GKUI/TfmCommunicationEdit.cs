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
	public class TfmCommunicationEdit : Form
	{
		private GroupBox GroupBox1;
		private TabControl PagesGroupData;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private Button btnAccept;
		private Button btnCancel;
		private Label Label1;
		private TextBox EditName;
		private Label Label4;
		private MaskedTextBox EditDate;
		private Label Label2;
		private ComboBox EditCorrType;
		private ComboBox EditDir;
		private Label Label5;
		private TextBox EditCorresponder;
		private Button btnPersonAdd;
		private TfmBase FBase;
		private TGEDCOMCommunicationRecord FCommunication;
		private TGEDCOMIndividualRecord FTempInd;
		private TSheetList FNotesList;
		private TSheetList FMediaList;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMCommunicationRecord Communication
		{
			get
			{
				return this.FCommunication;
			}
			set
			{
				this.SetCommunication(value);
			}
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FCommunication, ItemData as TGEDCOMNotes, Action))
				{
					this.ListsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList) && this.Base.ModifyRecMultimedia(this, this.FCommunication, ItemData as TGEDCOMMultimediaLink, Action))
				{
					this.ListsRefresh();
				}
			}
		}
		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FCommunication, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FCommunication, this.FMediaList.List, null);
		}
		private void SetCommunication([In] TGEDCOMCommunicationRecord Value)
		{
			this.FCommunication = Value;
			try
			{
				if (this.FCommunication == null)
				{
					this.EditName.Text = "";
					this.EditCorrType.SelectedIndex = -1;
					this.EditDate.Text = "";
					this.EditDir.SelectedIndex = 0;
					this.EditCorresponder.Text = "";
				}
				else
				{
					this.EditName.Text = this.FCommunication.Name;
					this.EditCorrType.SelectedIndex = (int)((sbyte)this.FCommunication.CommunicationType);
					this.EditDate.Text = TGenEngine.GEDCOMDateToStr(this.FCommunication.Date, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					TGEDCOMCommunicationRecord.TCommunicationDir dir = TGEDCOMCommunicationRecord.TCommunicationDir.cdFrom;
					this.FCommunication.GetCorresponder(ref dir, ref this.FTempInd);
					if (this.FTempInd != null)
					{
						this.EditDir.SelectedIndex = (int)((sbyte)dir);
						this.EditCorresponder.Text = TGenEngine.GetNameStr(this.FTempInd, true, false);
					}
					else
					{
						this.EditDir.SelectedIndex = 0;
						this.EditCorresponder.Text = "";
					}
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("CommunicationEdit.SetCommunication(): " + E.Message);
			}
		}
		private void InitializeComponent()
		{
			this.GroupBox1 = new GroupBox();
			this.Label1 = new Label();
			this.Label4 = new Label();
			this.Label2 = new Label();
			this.Label5 = new Label();
			this.btnPersonAdd = new Button();
			this.EditName = new TextBox();
			this.EditDate = new MaskedTextBox();
			this.EditCorrType = new ComboBox();
			this.EditDir = new ComboBox();
			this.EditCorresponder = new TextBox();
			this.PagesGroupData = new TabControl();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1.SuspendLayout();
			this.PagesGroupData.SuspendLayout();
			base.SuspendLayout();
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.btnPersonAdd);
			this.GroupBox1.Controls.Add(this.EditName);
			this.GroupBox1.Controls.Add(this.EditDate);
			this.GroupBox1.Controls.Add(this.EditCorrType);
			this.GroupBox1.Controls.Add(this.EditDir);
			this.GroupBox1.Controls.Add(this.EditCorresponder);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(481, 97);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.Label1.Location = new Point(8, 24);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(30, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Тема";
			this.Label4.Location = new Point(240, 72);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(30, 13);
			this.Label4.TabIndex = 1;
			this.Label4.Text = "Дата";
			this.Label2.Location = new Point(8, 72);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(25, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Тип";
			this.Label5.Location = new Point(8, 48);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(85, 13);
			this.Label5.TabIndex = 3;
			this.Label5.Text = "Корреспондент";
			this.btnPersonAdd.AccessibleDescription = "Выбрать персональную запись";
			this.btnPersonAdd.Location = new Point(448, 37);
			this.btnPersonAdd.Name = "btnPersonAdd";
			this.btnPersonAdd.Size = new Size(26, 26);
			this.btnPersonAdd.TabIndex = 4;
			this.btnPersonAdd.Click += new EventHandler(this.btnPersonAdd_Click);
			this.EditName.Location = new Point(96, 16);
			this.EditName.Name = "EditName";
			this.EditName.Size = new Size(377, 21);
			this.EditName.TabIndex = 0;
			this.EditName.Text = "";
			this.EditDate.Location = new Point(280, 64);
			this.EditDate.Mask = "00/00/0000";
			this.EditDate.MaxLength = 10;
			this.EditDate.Name = "EditDate";
			this.EditDate.Size = new Size(161, 21);
			this.EditDate.TabIndex = 4;
			this.EditDate.Text = "  .  .    ";
			this.EditCorrType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditCorrType.Location = new Point(96, 64);
			this.EditCorrType.Name = "EditCorrType";
			this.EditCorrType.Size = new Size(105, 21);
			this.EditCorrType.TabIndex = 3;
			this.EditDir.DropDownStyle = ComboBoxStyle.DropDownList;
			ComboBox.ObjectCollection arg_517_0 = this.EditDir.Items;
			object[] array = null;
			object[] array2 = array;
			object[] array3;
			object[] expr_4E6 = array3 = new object[2];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 2)
				{
					num = 2;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_4E6;
			array[0] = "от";
			array[1] = 'к';
			arg_517_0.AddRange(array);
			this.EditDir.Location = new Point(96, 40);
			this.EditDir.Name = "EditDir";
			this.EditDir.Size = new Size(65, 21);
			this.EditDir.TabIndex = 1;
			this.EditCorresponder.ForeColor = SystemColors.Control;
			this.EditCorresponder.Location = new Point(168, 40);
			this.EditCorresponder.Name = "EditCorresponder";
			this.EditCorresponder.ReadOnly = true;
			this.EditCorresponder.Size = new Size(273, 21);
			this.EditCorresponder.TabIndex = 2;
			this.EditCorresponder.Text = "";
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Controls.Add(this.SheetMultimedia);
			this.PagesGroupData.Location = new Point(0, 97);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new Size(481, 272);
			this.PagesGroupData.TabIndex = 0;
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(473, 246);
			this.SheetNotes.TabIndex = 0;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(473, 246);
			this.SheetMultimedia.TabIndex = 1;
			this.SheetMultimedia.Text = "Мультимедиа";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(304, 384);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(392, 384);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(481, 417);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.PagesGroupData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmCommunicationEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Редактирование коммуникации";
			this.GroupBox1.ResumeLayout(false);
			this.PagesGroupData.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FCommunication.Name = this.EditName.Text;
			this.FCommunication.CommunicationType = (TGEDCOMCommunicationRecord.TCommunicationType)this.EditCorrType.SelectedIndex;
			this.FCommunication.Date.ParseString(TGenEngine.StrToGEDCOMDate(this.EditDate.Text, true));
			this.FCommunication.SetCorresponder((TGEDCOMCommunicationRecord.TCommunicationDir)this.EditDir.SelectedIndex, this.FTempInd);
			this.Base.ChangeRecord(this.FCommunication);
			base.DialogResult = DialogResult.OK;
		}
		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.FTempInd = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svNone);
			this.EditCorresponder.Text = TGenEngine.GetNameStr(this.FTempInd, true, false);
		}
		public TfmCommunicationEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			TGEDCOMCommunicationRecord.TCommunicationType ct = TGEDCOMCommunicationRecord.TCommunicationType.ctCall;
			do
			{
				this.EditCorrType.Items.Add(GKL.LSList[(int)TGenEngine.CommunicationNames[(int)ct] - 1]);
				ct++;
			}
			while (ct != (TGEDCOMCommunicationRecord.TCommunicationType)6);
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecMediaList(this.FMediaList);
			this.FTempInd = null;
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[191];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
			this.Label1.Text = GKL.LSList[183];
			this.Label5.Text = GKL.LSList[184];
			this.Label2.Text = GKL.LSList[113];
			this.Label4.Text = GKL.LSList[139];
		}
	}
}
