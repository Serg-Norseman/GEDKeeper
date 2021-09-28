namespace GWTreePlugin
{
    partial class GWTreeForm
    {
        private GWTreeView gwTreeView;

        private void InitializeComponent()
        {
            this.gwTreeView = new GWTreeView();
            this.SuspendLayout();

            this.gwTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.gwTreeView.Location = new System.Drawing.Point(0, 0);
            this.gwTreeView.Name = "gwTreeView";
            this.gwTreeView.Size = new System.Drawing.Size(362, 266);
            this.gwTreeView.TabIndex = 0;

            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(362, 266);
            this.Controls.Add(this.gwTreeView);
            this.Name = "GWTreeForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "GWTreeForm";
            this.Closed += new System.EventHandler(this.GWTreeForm_Closed);
            this.Load += new System.EventHandler(this.GWTreeForm_Load);
            this.ResumeLayout(false);
        }
    }
}
