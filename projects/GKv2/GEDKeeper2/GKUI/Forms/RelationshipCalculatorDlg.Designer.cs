namespace GKUI.Forms
{
    partial class RelationshipCalculatorDlg
    {
        private System.ComponentModel.IContainer components = null;
        
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        private void InitializeComponent()
        {
            this.btnClose = new System.Windows.Forms.Button();
            this.Lab1 = new System.Windows.Forms.Label();
            this.Lab2 = new System.Windows.Forms.Label();
            this.Edit1 = new System.Windows.Forms.TextBox();
            this.Edit2 = new System.Windows.Forms.TextBox();
            this.btnRec1Select = new System.Windows.Forms.Button();
            this.btnRec2Select = new System.Windows.Forms.Button();
            this.txtResult = new System.Windows.Forms.TextBox();
            this.lblKinship = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // btnClose
            // 
            this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnClose.Location = new System.Drawing.Point(296, 250);
            this.btnClose.Margin = new System.Windows.Forms.Padding(2, 10, 2, 2);
            this.btnClose.Name = "btnClose";
            this.btnClose.Size = new System.Drawing.Size(91, 24);
            this.btnClose.TabIndex = 2;
            this.btnClose.Text = "btnClose";
            this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // Lab1
            // 
            this.Lab1.AutoSize = true;
            this.Lab1.Location = new System.Drawing.Point(12, 10);
            this.Lab1.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.Lab1.Name = "Lab1";
            this.Lab1.Size = new System.Drawing.Size(31, 13);
            this.Lab1.TabIndex = 15;
            this.Lab1.Text = "XXX1";
            // 
            // Lab2
            // 
            this.Lab2.AutoSize = true;
            this.Lab2.Location = new System.Drawing.Point(12, 58);
            this.Lab2.Margin = new System.Windows.Forms.Padding(2, 10, 2, 0);
            this.Lab2.Name = "Lab2";
            this.Lab2.Size = new System.Drawing.Size(31, 13);
            this.Lab2.TabIndex = 17;
            this.Lab2.Text = "XXX2";
            // 
            // Edit1
            // 
            this.Edit1.Location = new System.Drawing.Point(12, 25);
            this.Edit1.Margin = new System.Windows.Forms.Padding(2);
            this.Edit1.Name = "Edit1";
            this.Edit1.ReadOnly = true;
            this.Edit1.Size = new System.Drawing.Size(294, 21);
            this.Edit1.TabIndex = 16;
            // 
            // Edit2
            // 
            this.Edit2.Location = new System.Drawing.Point(12, 73);
            this.Edit2.Margin = new System.Windows.Forms.Padding(2);
            this.Edit2.Name = "Edit2";
            this.Edit2.ReadOnly = true;
            this.Edit2.Size = new System.Drawing.Size(294, 21);
            this.Edit2.TabIndex = 18;
            // 
            // btnRec1Select
            // 
            this.btnRec1Select.Location = new System.Drawing.Point(310, 24);
            this.btnRec1Select.Margin = new System.Windows.Forms.Padding(2);
            this.btnRec1Select.Name = "btnRec1Select";
            this.btnRec1Select.Size = new System.Drawing.Size(77, 22);
            this.btnRec1Select.TabIndex = 19;
            this.btnRec1Select.Text = "btnRec1Select";
            this.btnRec1Select.Click += new System.EventHandler(this.btnRec1Select_Click);
            // 
            // btnRec2Select
            // 
            this.btnRec2Select.Location = new System.Drawing.Point(310, 73);
            this.btnRec2Select.Margin = new System.Windows.Forms.Padding(2);
            this.btnRec2Select.Name = "btnRec2Select";
            this.btnRec2Select.Size = new System.Drawing.Size(77, 21);
            this.btnRec2Select.TabIndex = 20;
            this.btnRec2Select.Text = "btnRec2Select";
            this.btnRec2Select.Click += new System.EventHandler(this.btnRec2Select_Click);
            // 
            // txtResult
            // 
            this.txtResult.Location = new System.Drawing.Point(12, 122);
            this.txtResult.Multiline = true;
            this.txtResult.Name = "txtResult";
            this.txtResult.ReadOnly = true;
            this.txtResult.Size = new System.Drawing.Size(375, 115);
            this.txtResult.TabIndex = 22;
            // 
            // lblKinship
            // 
            this.lblKinship.AutoSize = true;
            this.lblKinship.Location = new System.Drawing.Point(12, 106);
            this.lblKinship.Margin = new System.Windows.Forms.Padding(2, 10, 2, 0);
            this.lblKinship.Name = "lblKinship";
            this.lblKinship.Size = new System.Drawing.Size(40, 13);
            this.lblKinship.TabIndex = 17;
            this.lblKinship.Text = "Kinship";
            // 
            // RelationshipCalculatorDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnClose;
            this.ClientSize = new System.Drawing.Size(399, 286);
            this.Controls.Add(this.txtResult);
            this.Controls.Add(this.Lab1);
            this.Controls.Add(this.lblKinship);
            this.Controls.Add(this.Lab2);
            this.Controls.Add(this.Edit1);
            this.Controls.Add(this.Edit2);
            this.Controls.Add(this.btnRec1Select);
            this.Controls.Add(this.btnRec2Select);
            this.Controls.Add(this.btnClose);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "RelationshipCalculatorDlg";
            this.Padding = new System.Windows.Forms.Padding(10);
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "RelationshipCalculator";
            this.ResumeLayout(false);
            this.PerformLayout();
        }
        private System.Windows.Forms.Label lblKinship;
        private System.Windows.Forms.TextBox txtResult;
        private System.Windows.Forms.Button btnRec2Select;
        private System.Windows.Forms.Button btnRec1Select;
        private System.Windows.Forms.TextBox Edit2;
        private System.Windows.Forms.TextBox Edit1;
        private System.Windows.Forms.Label Lab2;
        private System.Windows.Forms.Label Lab1;
        private System.Windows.Forms.Button btnClose;
    }
}
