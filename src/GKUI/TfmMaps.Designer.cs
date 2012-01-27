using System;

namespace GKUI
{
	partial class TfmMaps
	{
		private System.ComponentModel.IContainer components = null;
		private System.Windows.Forms.StatusBar StatusBar1;
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage tsPlaces;
		private System.Windows.Forms.TreeView TreePlaces;
		private System.Windows.Forms.SaveFileDialog SaveDialog1;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.GroupBox GroupBox2;
		private System.Windows.Forms.ComboBox ComboPersons;
		private System.Windows.Forms.CheckBox chkResidence;
		private System.Windows.Forms.CheckBox chkDeath;
		private System.Windows.Forms.CheckBox chkBirth;
		private System.Windows.Forms.Button btnSelectPlaces;
		private System.Windows.Forms.Button btnSaveImage;
		private System.Windows.Forms.RadioButton radTotal;
		private System.Windows.Forms.RadioButton radSelected;
		private System.Windows.Forms.CheckBox chkLinesVisible;

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FPlaces.Dispose();
				this.FMapPoints.Dispose();
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(Disposing);
		}

		private void InitializeComponent()
		{
			this.StatusBar1 = new System.Windows.Forms.StatusBar();
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.tsPlaces = new System.Windows.Forms.TabPage();
			this.TreePlaces = new System.Windows.Forms.TreeView();
			this.GroupBox2 = new System.Windows.Forms.GroupBox();
			this.ComboPersons = new System.Windows.Forms.ComboBox();
			this.chkResidence = new System.Windows.Forms.CheckBox();
			this.chkDeath = new System.Windows.Forms.CheckBox();
			this.chkBirth = new System.Windows.Forms.CheckBox();
			this.btnSelectPlaces = new System.Windows.Forms.Button();
			this.btnSaveImage = new System.Windows.Forms.Button();
			this.radTotal = new System.Windows.Forms.RadioButton();
			this.radSelected = new System.Windows.Forms.RadioButton();
			this.chkLinesVisible = new System.Windows.Forms.CheckBox();
			this.SaveDialog1 = new System.Windows.Forms.SaveFileDialog();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.PageControl1.SuspendLayout();
			this.tsPlaces.SuspendLayout();
			this.GroupBox2.SuspendLayout();
			base.SuspendLayout();
			this.StatusBar1.Location = new System.Drawing.Point(0, 485);
			this.StatusBar1.Name = "StatusBar1";
			this.StatusBar1.Size = new System.Drawing.Size(829, 19);
			this.StatusBar1.TabIndex = 3;
			this.PageControl1.Controls.Add(this.tsPlaces);
			this.PageControl1.Dock = System.Windows.Forms.DockStyle.Left;
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(289, 485);
			this.PageControl1.TabIndex = 1;
			this.tsPlaces.Controls.Add(this.TreePlaces);
			this.tsPlaces.Controls.Add(this.GroupBox2);
			this.tsPlaces.Location = new System.Drawing.Point(4, 22);
			this.tsPlaces.Name = "tsPlaces";
			this.tsPlaces.Size = new System.Drawing.Size(281, 459);
			this.tsPlaces.TabIndex = 0;
			this.tsPlaces.Text = "Места";
			this.TreePlaces.Dock = System.Windows.Forms.DockStyle.Fill;
			this.TreePlaces.ImageIndex = -1;
			this.TreePlaces.Location = new System.Drawing.Point(0, 185);
			this.TreePlaces.Name = "TreePlaces";
			this.TreePlaces.SelectedImageIndex = -1;
			this.TreePlaces.Size = new System.Drawing.Size(281, 274);
			this.TreePlaces.TabIndex = 0;
			this.TreePlaces.DoubleClick += new System.EventHandler(this.TreePlaces_DoubleClick);
			this.GroupBox2.Controls.Add(this.ComboPersons);
			this.GroupBox2.Controls.Add(this.chkResidence);
			this.GroupBox2.Controls.Add(this.chkDeath);
			this.GroupBox2.Controls.Add(this.chkBirth);
			this.GroupBox2.Controls.Add(this.btnSelectPlaces);
			this.GroupBox2.Controls.Add(this.btnSaveImage);
			this.GroupBox2.Controls.Add(this.radTotal);
			this.GroupBox2.Controls.Add(this.radSelected);
			this.GroupBox2.Controls.Add(this.chkLinesVisible);
			this.GroupBox2.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox2.Location = new System.Drawing.Point(0, 0);
			this.GroupBox2.Name = "GroupBox2";
			this.GroupBox2.Size = new System.Drawing.Size(281, 185);
			this.GroupBox2.TabIndex = 1;
			this.GroupBox2.TabStop = false;
			this.GroupBox2.Text = "Выборка";
			this.ComboPersons.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.ComboPersons.Location = new System.Drawing.Point(8, 104);
			this.ComboPersons.Name = "ComboPersons";
			this.ComboPersons.Size = new System.Drawing.Size(265, 21);
			this.ComboPersons.TabIndex = 5;
			this.chkResidence.Location = new System.Drawing.Point(19, 64);
			this.chkResidence.Name = "chkResidence";
			this.chkResidence.Size = new System.Drawing.Size(129, 17);
			this.chkResidence.TabIndex = 3;
			this.chkResidence.Text = "Места проживания";
			this.chkDeath.Location = new System.Drawing.Point(19, 48);
			this.chkDeath.Name = "chkDeath";
			this.chkDeath.Size = new System.Drawing.Size(129, 17);
			this.chkDeath.TabIndex = 2;
			this.chkDeath.Text = "Места смерти";
			this.chkBirth.Location = new System.Drawing.Point(19, 32);
			this.chkBirth.Name = "chkBirth";
			this.chkBirth.Size = new System.Drawing.Size(129, 17);
			this.chkBirth.TabIndex = 1;
			this.chkBirth.Text = "Места рождения";
			this.btnSelectPlaces.Enabled = false;
			this.btnSelectPlaces.Location = new System.Drawing.Point(198, 152);
			this.btnSelectPlaces.Name = "btnSelectPlaces";
			this.btnSelectPlaces.Size = new System.Drawing.Size(75, 25);
			this.btnSelectPlaces.TabIndex = 6;
			this.btnSelectPlaces.Text = "Показать";
			this.btnSelectPlaces.Click += new System.EventHandler(this.btnSelectPlaces_Click);
			this.btnSaveImage.Location = new System.Drawing.Point(8, 152);
			this.btnSaveImage.Name = "btnSaveImage";
			this.btnSaveImage.Size = new System.Drawing.Size(121, 25);
			this.btnSaveImage.TabIndex = 7;
			this.btnSaveImage.Text = "Сохранить снимок...";
			this.btnSaveImage.Click += new System.EventHandler(this.btnSaveImage_Click);
			this.radTotal.Location = new System.Drawing.Point(8, 16);
			this.radTotal.Name = "radTotal";
			this.radTotal.Size = new System.Drawing.Size(198, 17);
			this.radTotal.TabIndex = 0;
			this.radTotal.Text = "По всем людям";
			this.radTotal.Click += new System.EventHandler(this.radTotal_Click);
			this.radSelected.Location = new System.Drawing.Point(8, 87);
			this.radSelected.Name = "radSelected";
			this.radSelected.Size = new System.Drawing.Size(198, 17);
			this.radSelected.TabIndex = 4;
			this.radSelected.Text = "Только по выбранному";
			this.radSelected.Click += new System.EventHandler(this.radTotal_Click);
			this.chkLinesVisible.Checked = true;
			this.chkLinesVisible.CheckState = System.Windows.Forms.CheckState.Checked;
			this.chkLinesVisible.Location = new System.Drawing.Point(8, 128);
			this.chkLinesVisible.Name = "chkLinesVisible";
			this.chkLinesVisible.Size = new System.Drawing.Size(265, 17);
			this.chkLinesVisible.TabIndex = 8;
			this.chkLinesVisible.Text = "Отображать линии";
			this.SaveDialog1.DefaultExt = "jpg";
			this.SaveDialog1.Filter = "Image files|*.jpg";
			this.Panel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.Panel1.Location = new System.Drawing.Point(289, 0);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(540, 485);
			this.Panel1.TabIndex = 4;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			base.ClientSize = new System.Drawing.Size(829, 504);
			base.Controls.Add(this.Panel1);
			base.Controls.Add(this.PageControl1);
			base.Controls.Add(this.StatusBar1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			base.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			base.KeyPreview = true;
			base.Name = "TfmMaps";
			base.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Карты";
			base.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMaps_KeyDown);
			base.Load += new System.EventHandler(this.TfmMaps_Load);
			this.PageControl1.ResumeLayout(false);
			this.tsPlaces.ResumeLayout(false);
			this.GroupBox2.ResumeLayout(false);
			base.ResumeLayout(false);
		}
	}
}