/*
 * Создано в SharpDevelop.
 * Пользователь: TyulyakovIB
 * Дата: 09.01.2017
 * Время: 8:40
 * 
 * Для изменения этого шаблона используйте меню "Инструменты | Параметры | Кодирование | Стандартные заголовки".
 */
namespace GKUI.Controls
{
    partial class GKPortrait
    {
        /// <summary>
        /// Designer variable used to keep track of non-visual components.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.Panel btnPanel;    
        private System.Windows.Forms.Timer timer;
        /// <summary>
        /// Disposes resources used by the control.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        /// <summary>
        /// This method is required for Windows Forms designer support.
        /// Do not change the method contents inside the source code editor. The Forms designer might
        /// not be able to load this method if it was changed manually.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GKPortrait));
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.btnPanel = new System.Windows.Forms.Panel();
            this.timer = new System.Windows.Forms.Timer(this.components);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.SuspendLayout();
            // 
            // pictureBox1
            // 
            this.pictureBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pictureBox1.BackgroundImage")));
            this.pictureBox1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center;
            this.pictureBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pictureBox1.Location = new System.Drawing.Point(0, 0);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(178, 188);
            this.pictureBox1.TabIndex = 0;
            this.pictureBox1.TabStop = false;
            this.pictureBox1.MouseLeave += new System.EventHandler(this.PictureBox1MouseLeave);
            this.pictureBox1.MouseHover += new System.EventHandler(this.PictureBox1MouseHover);
            // 
            // panel1
            // 
            this.btnPanel.BackColor = System.Drawing.SystemColors.ButtonShadow;
            this.btnPanel.Location = new System.Drawing.Point(0, 152);
            this.btnPanel.Name = "panel1";
            this.btnPanel.Size = new System.Drawing.Size(178, 36);
            this.btnPanel.TabIndex = 1;
            this.btnPanel.MouseLeave += new System.EventHandler(this.Panel1MouseLeave);
            this.btnPanel.MouseHover += new System.EventHandler(this.Panel1MouseHover);
            // 
            // timer1
            // 
            this.timer.Tick += new System.EventHandler(this.MoveSlidePanel);
            // 
            // GKPortrait
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.btnPanel);
            this.Controls.Add(this.pictureBox1);
            this.Name = "GKPortrait";
            this.Size = new System.Drawing.Size(178, 188);
            this.Resize += new System.EventHandler(this.GKPortraitResize);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.ResumeLayout(false);

        }
    }
}
