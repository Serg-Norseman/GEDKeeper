namespace GWTreePlugin
{
    partial class GWTreeForm
    {
        private GWTreeView gwTreeView;

        private void InitializeComponent()
        {
            this.gwTreeView = new GWTreePlugin.GWTreeView();
            this.SuspendLayout();
            // 
            // gwTreeView
            // 
            this.gwTreeView.Context = null;
            this.gwTreeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.gwTreeView.Location = new System.Drawing.Point(0, 0);
            this.gwTreeView.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.gwTreeView.Name = "gwTreeView";
            this.gwTreeView.Size = new System.Drawing.Size(483, 327);
            this.gwTreeView.TabIndex = 0;
            // 
            // GWTreeForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1024, 768);
            this.Controls.Add(this.gwTreeView);
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.Name = "GWTreeForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "GWTreeForm";
            this.Closed += new System.EventHandler(this.GWTreeForm_Closed);
            this.Load += new System.EventHandler(this.GWTreeForm_Load);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.GWTreeForm_KeyDown);
            this.ResumeLayout(false);

        }
    }
}
