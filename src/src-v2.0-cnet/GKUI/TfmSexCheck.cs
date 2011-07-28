using GedCom551;
using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmSexCheck : Form
	{

		private TextBox edName;
		private GroupBox GroupBox1;
		private RadioButton sbNone;
		private RadioButton sbMale;
		private RadioButton sbFemale;
		private Button btnAccept;
		private Button btnCancel;

		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmSexCheck));
			this.edName = new TextBox();
			this.GroupBox1 = new GroupBox();
			this.sbNone = new RadioButton();
			this.sbMale = new RadioButton();
			this.sbFemale = new RadioButton();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1.SuspendLayout();
			base.SuspendLayout();
			this.edName.Location = new Point(8, 8);
			this.edName.Name = "edName";
			this.edName.ReadOnly = true;
			this.edName.Size = new Size(345, 21);
			this.edName.TabIndex = 0;
			this.edName.Text = "";
			this.GroupBox1.Controls.Add(this.sbNone);
			this.GroupBox1.Controls.Add(this.sbMale);
			this.GroupBox1.Controls.Add(this.sbFemale);
			this.GroupBox1.Location = new Point(8, 35);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(345, 49);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Пол";
			this.sbNone.Location = new Point(8, 16);
			this.sbNone.Name = "sbNone";
			this.sbNone.Size = new Size(105, 22);
			this.sbNone.TabIndex = 0;
			this.sbNone.Text = "?";
			this.sbMale.Location = new Point(119, 16);
			this.sbMale.Name = "sbMale";
			this.sbMale.Size = new Size(105, 22);
			this.sbMale.TabIndex = 1;
			this.sbMale.Text = "Мужской";
			this.sbFemale.Location = new Point(231, 16);
			this.sbFemale.Name = "sbFemale";
			this.sbFemale.Size = new Size(105, 22);
			this.sbFemale.TabIndex = 2;
			this.sbFemale.Text = "Женский";
			this.btnAccept.DialogResult = DialogResult.OK;
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(184, 96);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(272, 96);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(361, 130);
			base.Controls.Add(this.edName);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedSingle;
			base.Name = "TfmSexCheck";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Проверка пола";
			this.GroupBox1.ResumeLayout(false);
			base.ResumeLayout(false);
		}

		public TfmSexCheck()
		{
			this.InitializeComponent();
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[104];
			this.GroupBox1.Text = GKL.LSList[87];
			this.sbMale.Text = GKL.LSList[66];
			this.sbFemale.Text = GKL.LSList[67];
		}


		public static TGEDCOMObject.TGEDCOMSex DefineSex(string iName, string iPatr, TNamesTable aNamesTable)
		{
			TGEDCOMObject.TGEDCOMSex sx = aNamesTable.GetSexByName(iName);
			TGEDCOMObject.TGEDCOMSex Result = sx;
			if (sx == TGEDCOMObject.TGEDCOMSex.svNone)
			{
				TfmSexCheck dlg = new TfmSexCheck();
				try
				{
					if (dlg != null)
					{
						dlg.edName.Text = iName + " " + iPatr;
						sx = TGenEngine.GetSex(iName, iPatr, false);
						if (sx != TGEDCOMObject.TGEDCOMSex.svNone)
						{
							if (sx == TGEDCOMObject.TGEDCOMSex.svMale)
							{
								dlg.sbMale.Checked = true;
								goto IL_78;
							}
							if (sx == TGEDCOMObject.TGEDCOMSex.svFemale)
							{
								dlg.sbFemale.Checked = true;
								goto IL_78;
							}
							if (sx != TGEDCOMObject.TGEDCOMSex.svUndetermined)
							{
								goto IL_78;
							}
						}
						dlg.sbNone.Checked = true;
						IL_78:
						if (dlg.ShowDialog() == DialogResult.OK)
						{
							if (dlg.sbNone.Checked)
							{
								sx = TGEDCOMObject.TGEDCOMSex.svNone;
							}
							else
							{
								if (dlg.sbMale.Checked)
								{
									sx = TGEDCOMObject.TGEDCOMSex.svMale;
								}
								else
								{
									if (dlg.sbFemale.Checked)
									{
										sx = TGEDCOMObject.TGEDCOMSex.svFemale;
									}
								}
							}
							Result = sx;
							if (sx != TGEDCOMObject.TGEDCOMSex.svNone)
							{
								aNamesTable.SetNameSex(iName, sx);
							}
						}
					}
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			return Result;
		}


		public static void CheckPersonSex(TGEDCOMIndividualRecord iRec, TNamesTable aNamesTable)
		{
			TGEDCOMObject.TGEDCOMSex sex = iRec.Sex;
			if (sex == TGEDCOMObject.TGEDCOMSex.svNone || sex == TGEDCOMObject.TGEDCOMSex.svUndetermined)
			{
				string f_fam = "";
				string f_name = "";
				string f_patr = "";
				TGenEngine.GetNameParts(iRec, ref f_fam, ref f_name, ref f_patr);
				iRec.Sex = DefineSex(f_name, f_patr, aNamesTable);
			}
		}

	}
}
