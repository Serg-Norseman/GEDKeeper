namespace GKTreeVizPlugin
{
    partial class TVSettingsDlg
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.NumericUpDown edMinGens;
        private System.Windows.Forms.Label lblMinGens;
        private System.Windows.Forms.CheckBox chkWithoutDates;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
        	this.chkWithoutDates = new System.Windows.Forms.CheckBox();
        	this.lblMinGens = new System.Windows.Forms.Label();
        	this.edMinGens = new System.Windows.Forms.NumericUpDown();
        	this.btnAccept = new System.Windows.Forms.Button();
        	this.btnCancel = new System.Windows.Forms.Button();
        	((System.ComponentModel.ISupportInitialize)(this.edMinGens)).BeginInit();
        	this.SuspendLayout();
        	// 
        	// chkWithoutDates
        	// 
        	this.chkWithoutDates.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
        	this.chkWithoutDates.Location = new System.Drawing.Point(15, 42);
        	this.chkWithoutDates.Name = "chkWithoutDates";
        	this.chkWithoutDates.Size = new System.Drawing.Size(401, 38);
        	this.chkWithoutDates.TabIndex = 8;
        	this.chkWithoutDates.Text = "chkWithoutDates";
        	this.chkWithoutDates.UseVisualStyleBackColor = true;
        	// 
        	// lblMinGens
        	// 
        	this.lblMinGens.Location = new System.Drawing.Point(13, 18);
        	this.lblMinGens.Name = "lblMinGens";
        	this.lblMinGens.Size = new System.Drawing.Size(286, 21);
        	this.lblMinGens.TabIndex = 6;
        	this.lblMinGens.Text = "lblMinGens";
        	// 
        	// edMinGens
        	// 
        	this.edMinGens.Location = new System.Drawing.Point(305, 12);
        	this.edMinGens.Name = "edMinGens";
        	this.edMinGens.Size = new System.Drawing.Size(111, 24);
        	this.edMinGens.TabIndex = 7;
        	this.edMinGens.Value = new decimal(new int[] {
        	        	        	2,
        	        	        	0,
        	        	        	0,
        	        	        	0});
        	// 
        	// btnAccept
        	// 
        	this.btnAccept.DialogResult = System.Windows.Forms.DialogResult.OK;
        	this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
        	this.btnAccept.Location = new System.Drawing.Point(184, 86);
        	this.btnAccept.Name = "btnAccept";
        	this.btnAccept.Size = new System.Drawing.Size(113, 30);
        	this.btnAccept.TabIndex = 2;
        	this.btnAccept.Text = "btnAccept";
        	this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
        	this.btnAccept.Click += new System.EventHandler(this.BtnAcceptClick);
        	// 
        	// btnCancel
        	// 
        	this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
        	this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
        	this.btnCancel.Location = new System.Drawing.Point(303, 86);
        	this.btnCancel.Name = "btnCancel";
        	this.btnCancel.Size = new System.Drawing.Size(113, 30);
        	this.btnCancel.TabIndex = 3;
        	this.btnCancel.Text = "btnCancel";
        	this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
        	// 
        	// frmTVPSettings
        	// 
        	this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
        	this.ClientSize = new System.Drawing.Size(428, 131);
        	this.Controls.Add(this.chkWithoutDates);
        	this.Controls.Add(this.lblMinGens);
        	this.Controls.Add(this.edMinGens);
        	this.Controls.Add(this.btnAccept);
        	this.Controls.Add(this.btnCancel);
        	this.Font = new System.Drawing.Font("Tahoma", 8.25F);
        	this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
        	this.MaximizeBox = false;
        	this.MinimizeBox = false;
        	this.Name = "TVSettingsDlg";
        	this.ShowInTaskbar = false;
        	this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
        	this.Text = "TVSettingsDlg";
        	((System.ComponentModel.ISupportInitialize)(this.edMinGens)).EndInit();
        	this.ResumeLayout(false);
        }

        #endregion
    }
}