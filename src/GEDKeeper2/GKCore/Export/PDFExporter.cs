using System;
using System.IO;
using System.Windows.Forms;

using GKCore.Interfaces;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class PDFExporter : Exporter
	{
		private Padding fMargins;
		protected Document fDocument;
		protected PdfWriter fWriter;
		protected bool fAlbumPage;

	    protected PDFExporter(IBaseWindow aBase) : base(aBase)
		{
			this.fMargins.Left = 20;
			this.fMargins.Top = 20;
			this.fMargins.Right = 20;
			this.fMargins.Bottom = 20;
			this.fAlbumPage = false;
		}

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fDocument != null) fDocument.Dispose();
            }
            base.Dispose(disposing);
        }

        protected abstract void InternalGenerate();

		public override void Generate(bool show)
		{
			bool success = false;
			if (!this.IsRequireFilename("PDF files (*.pdf)|*.pdf")) return;

			Rectangle pageSize = !this.fAlbumPage ? PageSize.A4 : PageSize.A4.Rotate();

			fDocument = new Document(pageSize, this.fMargins.Left, this.fMargins.Right, this.fMargins.Top, this.fMargins.Bottom);
			try
			{
				try
				{
					this.fWriter = PdfWriter.GetInstance(fDocument, new FileStream(this.fPath, FileMode.Create));
					this.InternalGenerate();
					success = true;
				}
				finally
				{
					fDocument.Close();
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("PDFExporter.Generate(): " + ex.Message);
				this.fBase.Host.LogWrite("PDFExporter.Generate(): " + ex.StackTrace);
			}

			if (!success) {
				MessageBox.Show(LangMan.LS(LSID.LSID_GenerationFailed));
			} else {
				if (show) this.ShowResult();
			}
		}

		protected void AddParagraph(Chunk chunk, int alignment = Element.ALIGN_LEFT)
		{
			fDocument.Add(new Paragraph(chunk) { Alignment = alignment });
		}
	}
}
