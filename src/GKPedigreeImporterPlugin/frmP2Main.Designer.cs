namespace GKPedigreeImporterPlugin
{
    partial class frmP2Main
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

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
        	System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(frmP2Main));
        	this.Label3 = new System.Windows.Forms.Label();
        	this.edImportFile = new System.Windows.Forms.TextBox();
        	this.btnImportFileChoose = new System.Windows.Forms.Button();
        	this.ListBox1 = new System.Windows.Forms.ListBox();
        	this.OpenDialog2 = new System.Windows.Forms.OpenFileDialog();
        	this.SuspendLayout();
        	// 
        	// Label3
        	// 
        	this.Label3.Location = new System.Drawing.Point(11, 20);
        	this.Label3.Name = "Label3";
        	this.Label3.Size = new System.Drawing.Size(41, 13);
        	this.Label3.TabIndex = 4;
        	this.Label3.Text = "Файл";
        	// 
        	// edImportFile
        	// 
        	this.edImportFile.Location = new System.Drawing.Point(58, 12);
        	this.edImportFile.Name = "edImportFile";
        	this.edImportFile.ReadOnly = true;
        	this.edImportFile.Size = new System.Drawing.Size(562, 20);
        	this.edImportFile.TabIndex = 3;
        	// 
        	// btnImportFileChoose
        	// 
        	this.btnImportFileChoose.Location = new System.Drawing.Point(627, 10);
        	this.btnImportFileChoose.Name = "btnImportFileChoose";
        	this.btnImportFileChoose.Size = new System.Drawing.Size(81, 25);
        	this.btnImportFileChoose.TabIndex = 5;
        	this.btnImportFileChoose.Text = "Выбрать...";
        	// 
        	// ListBox1
        	// 
        	this.ListBox1.Location = new System.Drawing.Point(11, 44);
        	this.ListBox1.Name = "ListBox1";
        	this.ListBox1.Size = new System.Drawing.Size(697, 342);
        	this.ListBox1.TabIndex = 6;
        	// 
        	// OpenDialog2
        	// 
        	this.OpenDialog2.Filter = resources.GetString("OpenDialog2.Filter");
        	// 
        	// frmP2Main
        	// 
        	this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        	this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        	this.ClientSize = new System.Drawing.Size(722, 399);
        	this.Controls.Add(this.Label3);
        	this.Controls.Add(this.edImportFile);
        	this.Controls.Add(this.btnImportFileChoose);
        	this.Controls.Add(this.ListBox1);
        	this.MaximizeBox = false;
        	this.MinimizeBox = false;
        	this.Name = "frmP2Main";
        	this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
        	this.Text = "frmP2Main";
        	this.ResumeLayout(false);
        	this.PerformLayout();
        }
        private System.Windows.Forms.OpenFileDialog OpenDialog2;
        private System.Windows.Forms.ListBox ListBox1;
        private System.Windows.Forms.Button btnImportFileChoose;
        private System.Windows.Forms.TextBox edImportFile;
        private System.Windows.Forms.Label Label3;

        #endregion

    }
}