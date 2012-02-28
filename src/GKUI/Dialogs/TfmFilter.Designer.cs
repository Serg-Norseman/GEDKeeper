using System;

namespace GKUI
{
	partial class TfmFilter
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox rgLife;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.ComboBox edName;
		private System.Windows.Forms.GroupBox rgSex;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.MaskedTextBox edAliveBeforeDate;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.CheckBox CheckPatriarch;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.ComboBox cbResidence;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.ComboBox cbGroup;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.ComboBox cbSource;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.ComboBox cbEventVal;
		private System.Windows.Forms.RadioButton RadioButton1;
		private System.Windows.Forms.RadioButton RadioButton2;
		private System.Windows.Forms.RadioButton RadioButton3;
		private System.Windows.Forms.RadioButton RadioButton4;
		private System.Windows.Forms.RadioButton RadioButton5;
		private System.Windows.Forms.RadioButton RadioButton6;
		private System.Windows.Forms.RadioButton RadioButton7;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.rgLife = new System.Windows.Forms.GroupBox();
			this.RadioButton4 = new System.Windows.Forms.RadioButton();
			this.RadioButton3 = new System.Windows.Forms.RadioButton();
			this.RadioButton2 = new System.Windows.Forms.RadioButton();
			this.RadioButton1 = new System.Windows.Forms.RadioButton();
			this.Label1 = new System.Windows.Forms.Label();
			this.edName = new System.Windows.Forms.ComboBox();
			this.rgSex = new System.Windows.Forms.GroupBox();
			this.RadioButton7 = new System.Windows.Forms.RadioButton();
			this.RadioButton5 = new System.Windows.Forms.RadioButton();
			this.RadioButton6 = new System.Windows.Forms.RadioButton();
			this.Label2 = new System.Windows.Forms.Label();
			this.edAliveBeforeDate = new System.Windows.Forms.MaskedTextBox();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.CheckPatriarch = new System.Windows.Forms.CheckBox();
			this.Label3 = new System.Windows.Forms.Label();
			this.cbResidence = new System.Windows.Forms.ComboBox();
			this.Label4 = new System.Windows.Forms.Label();
			this.cbGroup = new System.Windows.Forms.ComboBox();
			this.Label5 = new System.Windows.Forms.Label();
			this.cbSource = new System.Windows.Forms.ComboBox();
			this.Label6 = new System.Windows.Forms.Label();
			this.cbEventVal = new System.Windows.Forms.ComboBox();
			this.rgLife.SuspendLayout();
			this.rgSex.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(120, 464);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 15;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(208, 464);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 16;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			// 
			// rgLife
			// 
			this.rgLife.Controls.Add(this.RadioButton4);
			this.rgLife.Controls.Add(this.RadioButton3);
			this.rgLife.Controls.Add(this.RadioButton2);
			this.rgLife.Controls.Add(this.RadioButton1);
			this.rgLife.Location = new System.Drawing.Point(8, 8);
			this.rgLife.Name = "rgLife";
			this.rgLife.Size = new System.Drawing.Size(137, 104);
			this.rgLife.TabIndex = 0;
			this.rgLife.TabStop = false;
			// 
			// RadioButton4
			// 
			this.RadioButton4.Location = new System.Drawing.Point(8, 74);
			this.RadioButton4.Name = "RadioButton4";
			this.RadioButton4.Size = new System.Drawing.Size(114, 24);
			this.RadioButton4.TabIndex = 3;
			this.RadioButton4.Text = "в живых до";
			this.RadioButton4.Click += new System.EventHandler(this.rgLifeClick);
			// 
			// RadioButton3
			// 
			this.RadioButton3.Location = new System.Drawing.Point(8, 56);
			this.RadioButton3.Name = "RadioButton3";
			this.RadioButton3.Size = new System.Drawing.Size(114, 24);
			this.RadioButton3.TabIndex = 2;
			this.RadioButton3.Text = "только умершие";
			this.RadioButton3.Click += new System.EventHandler(this.rgLifeClick);
			// 
			// RadioButton2
			// 
			this.RadioButton2.Location = new System.Drawing.Point(8, 38);
			this.RadioButton2.Name = "RadioButton2";
			this.RadioButton2.Size = new System.Drawing.Size(114, 24);
			this.RadioButton2.TabIndex = 1;
			this.RadioButton2.Text = "только живые";
			this.RadioButton2.Click += new System.EventHandler(this.rgLifeClick);
			// 
			// RadioButton1
			// 
			this.RadioButton1.Location = new System.Drawing.Point(8, 20);
			this.RadioButton1.Name = "RadioButton1";
			this.RadioButton1.Size = new System.Drawing.Size(104, 24);
			this.RadioButton1.TabIndex = 0;
			this.RadioButton1.Text = "все";
			this.RadioButton1.Click += new System.EventHandler(this.rgLifeClick);
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 168);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(75, 13);
			this.Label1.TabIndex = 4;
			this.Label1.Text = "Маска имени";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(8, 184);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(281, 21);
			this.edName.Sorted = true;
			this.edName.TabIndex = 5;
			this.edName.Text = "*";
			// 
			// rgSex
			// 
			this.rgSex.Controls.Add(this.RadioButton7);
			this.rgSex.Controls.Add(this.RadioButton5);
			this.rgSex.Controls.Add(this.RadioButton6);
			this.rgSex.Location = new System.Drawing.Point(152, 8);
			this.rgSex.Name = "rgSex";
			this.rgSex.Size = new System.Drawing.Size(137, 104);
			this.rgSex.TabIndex = 1;
			this.rgSex.TabStop = false;
			// 
			// RadioButton7
			// 
			this.RadioButton7.Location = new System.Drawing.Point(8, 72);
			this.RadioButton7.Name = "RadioButton7";
			this.RadioButton7.Size = new System.Drawing.Size(114, 24);
			this.RadioButton7.TabIndex = 2;
			this.RadioButton7.Text = "только женщины";
			// 
			// RadioButton5
			// 
			this.RadioButton5.Location = new System.Drawing.Point(8, 24);
			this.RadioButton5.Name = "RadioButton5";
			this.RadioButton5.Size = new System.Drawing.Size(104, 24);
			this.RadioButton5.TabIndex = 0;
			this.RadioButton5.Text = "все";
			// 
			// RadioButton6
			// 
			this.RadioButton6.Location = new System.Drawing.Point(8, 48);
			this.RadioButton6.Name = "RadioButton6";
			this.RadioButton6.Size = new System.Drawing.Size(114, 24);
			this.RadioButton6.TabIndex = 1;
			this.RadioButton6.Text = "только мужчины";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 120);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(70, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "В живых до:";
			// 
			// edAliveBeforeDate
			// 
			this.edAliveBeforeDate.Enabled = false;
			this.edAliveBeforeDate.Location = new System.Drawing.Point(8, 136);
			this.edAliveBeforeDate.Mask = "00/00/0000";
			this.edAliveBeforeDate.Name = "edAliveBeforeDate";
			this.edAliveBeforeDate.Size = new System.Drawing.Size(137, 21);
			this.edAliveBeforeDate.TabIndex = 3;
			this.edAliveBeforeDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.CheckPatriarch);
			this.GroupBox1.Location = new System.Drawing.Point(8, 408);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(281, 41);
			this.GroupBox1.TabIndex = 14;
			this.GroupBox1.TabStop = false;
			// 
			// CheckPatriarch
			// 
			this.CheckPatriarch.Location = new System.Drawing.Point(8, 16);
			this.CheckPatriarch.Name = "CheckPatriarch";
			this.CheckPatriarch.Size = new System.Drawing.Size(185, 17);
			this.CheckPatriarch.TabIndex = 0;
			this.CheckPatriarch.Text = "Только главы семей";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 216);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(175, 13);
			this.Label3.TabIndex = 6;
			this.Label3.Text = "Маска местожительства";
			// 
			// cbResidence
			// 
			this.cbResidence.Location = new System.Drawing.Point(8, 232);
			this.cbResidence.Name = "cbResidence";
			this.cbResidence.Size = new System.Drawing.Size(281, 21);
			this.cbResidence.Sorted = true;
			this.cbResidence.TabIndex = 7;
			this.cbResidence.Text = "*";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(8, 312);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(45, 13);
			this.Label4.TabIndex = 10;
			this.Label4.Text = "Группы";
			// 
			// cbGroup
			// 
			this.cbGroup.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbGroup.Location = new System.Drawing.Point(8, 328);
			this.cbGroup.Name = "cbGroup";
			this.cbGroup.Size = new System.Drawing.Size(281, 21);
			this.cbGroup.TabIndex = 11;
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(8, 360);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(60, 13);
			this.Label5.TabIndex = 12;
			this.Label5.Text = "Источники";
			// 
			// cbSource
			// 
			this.cbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbSource.Location = new System.Drawing.Point(8, 376);
			this.cbSource.Name = "cbSource";
			this.cbSource.Size = new System.Drawing.Size(281, 21);
			this.cbSource.TabIndex = 13;
			// 
			// Label6
			// 
			this.Label6.Location = new System.Drawing.Point(8, 264);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(85, 13);
			this.Label6.TabIndex = 8;
			this.Label6.Text = "Маска фактов";
			// 
			// cbEventVal
			// 
			this.cbEventVal.Location = new System.Drawing.Point(8, 280);
			this.cbEventVal.Name = "cbEventVal";
			this.cbEventVal.Size = new System.Drawing.Size(281, 21);
			this.cbEventVal.Sorted = true;
			this.cbEventVal.TabIndex = 9;
			this.cbEventVal.Text = "*";
			// 
			// TfmFilter
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(297, 497);
			this.Controls.Add(this.Label1);
			this.Controls.Add(this.Label2);
			this.Controls.Add(this.Label3);
			this.Controls.Add(this.Label4);
			this.Controls.Add(this.Label5);
			this.Controls.Add(this.Label6);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.rgLife);
			this.Controls.Add(this.edName);
			this.Controls.Add(this.rgSex);
			this.Controls.Add(this.edAliveBeforeDate);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.cbResidence);
			this.Controls.Add(this.cbGroup);
			this.Controls.Add(this.cbSource);
			this.Controls.Add(this.cbEventVal);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmFilter";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Фильтр";
			this.Load += new System.EventHandler(this.TfmFilter_Load);
			this.rgLife.ResumeLayout(false);
			this.rgSex.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}