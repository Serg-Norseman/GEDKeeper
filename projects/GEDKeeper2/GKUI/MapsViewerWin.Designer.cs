using System;

namespace GKUI
{
	partial class MapsViewerWin
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
				this.fPlaces.Dispose();
				this.fMapPoints.Dispose();
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
			this.SuspendLayout();
			// 
			// StatusBar1
			// 
			this.StatusBar1.Location = new System.Drawing.Point(0, 549);
			this.StatusBar1.Name = "StatusBar1";
			this.StatusBar1.Size = new System.Drawing.Size(1101, 23);
			this.StatusBar1.TabIndex = 3;
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.tsPlaces);
			this.PageControl1.Dock = System.Windows.Forms.DockStyle.Left;
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(405, 549);
			this.PageControl1.TabIndex = 1;
			// 
			// tsPlaces
			// 
			this.tsPlaces.Controls.Add(this.TreePlaces);
			this.tsPlaces.Controls.Add(this.GroupBox2);
			this.tsPlaces.Location = new System.Drawing.Point(4, 26);
			this.tsPlaces.Name = "tsPlaces";
			this.tsPlaces.Size = new System.Drawing.Size(397, 519);
			this.tsPlaces.TabIndex = 0;
			this.tsPlaces.Text = "Места";
			// 
			// TreePlaces
			// 
			this.TreePlaces.Dock = System.Windows.Forms.DockStyle.Fill;
			this.TreePlaces.Location = new System.Drawing.Point(0, 225);
			this.TreePlaces.Name = "TreePlaces";
			this.TreePlaces.Size = new System.Drawing.Size(397, 294);
			this.TreePlaces.TabIndex = 0;
			this.TreePlaces.DoubleClick += new System.EventHandler(this.TreePlaces_DoubleClick);
			// 
			// GroupBox2
			// 
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
			this.GroupBox2.Size = new System.Drawing.Size(397, 225);
			this.GroupBox2.TabIndex = 1;
			this.GroupBox2.TabStop = false;
			this.GroupBox2.Text = "Выборка";
			// 
			// ComboPersons
			// 
			this.ComboPersons.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.ComboPersons.Location = new System.Drawing.Point(11, 126);
			this.ComboPersons.Name = "ComboPersons";
			this.ComboPersons.Size = new System.Drawing.Size(371, 25);
			this.ComboPersons.TabIndex = 5;
			// 
			// chkResidence
			// 
			this.chkResidence.AutoSize = true;
			this.chkResidence.Location = new System.Drawing.Point(27, 78);
			this.chkResidence.Name = "chkResidence";
			this.chkResidence.Size = new System.Drawing.Size(152, 21);
			this.chkResidence.TabIndex = 3;
			this.chkResidence.Text = "Места проживания";
			// 
			// chkDeath
			// 
			this.chkDeath.AutoSize = true;
			this.chkDeath.Location = new System.Drawing.Point(27, 58);
			this.chkDeath.Name = "chkDeath";
			this.chkDeath.Size = new System.Drawing.Size(120, 21);
			this.chkDeath.TabIndex = 2;
			this.chkDeath.Text = "Места смерти";
			// 
			// chkBirth
			// 
			this.chkBirth.AutoSize = true;
			this.chkBirth.Location = new System.Drawing.Point(27, 39);
			this.chkBirth.Name = "chkBirth";
			this.chkBirth.Size = new System.Drawing.Size(137, 21);
			this.chkBirth.TabIndex = 1;
			this.chkBirth.Text = "Места рождения";
			// 
			// btnSelectPlaces
			// 
			this.btnSelectPlaces.Enabled = false;
			this.btnSelectPlaces.Location = new System.Drawing.Point(277, 185);
			this.btnSelectPlaces.Name = "btnSelectPlaces";
			this.btnSelectPlaces.Size = new System.Drawing.Size(105, 30);
			this.btnSelectPlaces.TabIndex = 6;
			this.btnSelectPlaces.Text = "Показать";
			this.btnSelectPlaces.Click += new System.EventHandler(this.btnSelectPlaces_Click);
			// 
			// btnSaveImage
			// 
			this.btnSaveImage.Location = new System.Drawing.Point(11, 185);
			this.btnSaveImage.Name = "btnSaveImage";
			this.btnSaveImage.Size = new System.Drawing.Size(170, 30);
			this.btnSaveImage.TabIndex = 7;
			this.btnSaveImage.Text = "Сохранить снимок...";
			this.btnSaveImage.Click += new System.EventHandler(this.btnSaveImage_Click);
			// 
			// radTotal
			// 
			this.radTotal.AutoSize = true;
			this.radTotal.Location = new System.Drawing.Point(11, 19);
			this.radTotal.Name = "radTotal";
			this.radTotal.Size = new System.Drawing.Size(127, 21);
			this.radTotal.TabIndex = 0;
			this.radTotal.Text = "По всем людям";
			this.radTotal.Click += new System.EventHandler(this.radTotal_Click);
			// 
			// radSelected
			// 
			this.radSelected.AutoSize = true;
			this.radSelected.Location = new System.Drawing.Point(11, 106);
			this.radSelected.Name = "radSelected";
			this.radSelected.Size = new System.Drawing.Size(180, 21);
			this.radSelected.TabIndex = 4;
			this.radSelected.Text = "Только по выбранному";
			this.radSelected.Click += new System.EventHandler(this.radTotal_Click);
			// 
			// chkLinesVisible
			// 
			this.chkLinesVisible.AutoSize = true;
			this.chkLinesVisible.Checked = true;
			this.chkLinesVisible.CheckState = System.Windows.Forms.CheckState.Checked;
			this.chkLinesVisible.Location = new System.Drawing.Point(11, 155);
			this.chkLinesVisible.Name = "chkLinesVisible";
			this.chkLinesVisible.Size = new System.Drawing.Size(155, 21);
			this.chkLinesVisible.TabIndex = 8;
			this.chkLinesVisible.Text = "Отображать линии";
			// 
			// SaveDialog1
			// 
			this.SaveDialog1.DefaultExt = "jpg";
			this.SaveDialog1.Filter = "Image files|*.jpg";
			// 
			// Panel1
			// 
			this.Panel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.Panel1.Location = new System.Drawing.Point(405, 0);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(696, 549);
			this.Panel1.TabIndex = 4;
			// 
			// MapsViewerWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(1101, 572);
			this.Controls.Add(this.Panel1);
			this.Controls.Add(this.PageControl1);
			this.Controls.Add(this.StatusBar1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.KeyPreview = true;
			this.Name = "MapsViewerWin";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Карты";
			this.Load += new System.EventHandler(this.TfmMaps_Load);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMaps_KeyDown);
			this.PageControl1.ResumeLayout(false);
			this.tsPlaces.ResumeLayout(false);
			this.GroupBox2.ResumeLayout(false);
			this.GroupBox2.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}