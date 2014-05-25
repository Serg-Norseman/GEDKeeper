using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Widgets
{
	public partial class TfmNamesBook : Form, IWidget
	{
    	#region IWidget common

    	private IHost fHost;
    	private MenuItem fMenuItem;

    	IHost IWidget.Host
    	{
    		get { return this.fHost; }
    	}

    	MenuItem IWidget.MenuItem
    	{
    		get { return this.fMenuItem; }
    	}

    	void IWidget.WidgetInit(IHost host, MenuItem menuItem)
    	{
    		this.fHost = host;
    		this.fMenuItem = menuItem;
    	}

        void IWidget.BaseChanged(IBase aBase) {}

        #endregion

		private class TNameRecord
		{
			public string Name;
			public string Desc;
			public TGEDCOMSex Sex;
			public int ChIndex;
		}

		private readonly List<TNameRecord> fNames;
		private readonly StringList fChurchFNames;
		private readonly StringList fChurchMNames;

        public TfmNamesBook() : base()
        {
            this.InitializeComponent();

            Screen scr = Screen.PrimaryScreen;
            this.Location = new Point(scr.WorkingArea.Width - this.Width - 10, (scr.WorkingArea.Height - this.Height) / 2);

            this.fNames = new List<TNameRecord>();
            this.fChurchFNames = new StringList();
            this.fChurchMNames = new StringList();

            this.PrepareList();
            this.UpdateList();

            this.Text = LangMan.LS(LSID.LSID_MINamesBook);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fChurchFNames.Dispose();
                this.fChurchMNames.Dispose();
                //this.fNames.Dispose();
            }
            base.Dispose(disposing);
        }

        private void TfmNamesBook_Load(object sender, EventArgs e)
        {
        	this.fHost.WidgetShow(this);
        }

		private void TfmNamesBook_Closed(object sender, EventArgs e)
		{
			this.fHost.WidgetClose(this);
		}

		private void cbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			int idx = this.cbNames.SelectedIndex;
			if (idx >= 0 && idx < this.cbNames.Items.Count)
			{
			    GKComboItem item = (this.cbNames.Items[idx] as GKComboItem);
				TNameRecord rec = item.Data as TNameRecord;

				this.mmDesc.Text = "";
				this.mmDesc.AppendText(rec.Name + "\r\n");
				this.mmDesc.AppendText(rec.Desc + "\r\n");

                if (rec.ChIndex >= 0)
				{
					this.mmDesc.AppendText("\r\n");
					this.mmDesc.AppendText("Святцы:\r\n");
					
                    StringList lst = null;
					if (rec.Sex == TGEDCOMSex.svMale)
					{
						lst = this.fChurchMNames;
					}
					else
					{
						if (rec.Sex == TGEDCOMSex.svFemale)
						{
							lst = this.fChurchFNames;
						}
					}

					int num = lst.Count - 1;
					for (int i = rec.ChIndex + 1; i <= num; i++)
					{
						string st = lst[i].Trim();
						if (st[0] == '-')
						{
							break;
						}
						st = st.Remove(0, 1);
						this.mmDesc.AppendText(st + "\r\n");
					}
				}
			}
		}

		private static bool ExtractFlags(ref string st)
		{
			bool res = (st != null);
			if (res) {
				res = (st.Length >= 2 && st[0] == '[' && st[st.Length - 1] == ']');
				if (res) {
					st = st.Substring(1, st.Length - 2);
				}
			}
			return res;
		}

		private void PrepareList()
		{
		    using (MemoryStream memStream = new MemoryStream(GKResources.book_names))
		    {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        if (ns != "")
                        {
                            string[] toks = ns.Split('/');
                            if (toks.Length >= 3)
                            {
                                TNameRecord rec = new TNameRecord();

                                rec.Name = toks[0].Trim();
                                rec.Desc = toks[2].Trim();
                                string st = toks[1].Trim();

                                if (ExtractFlags(ref st))
                                {
                                    char c = st[0];
                                    switch (c)
                                    {
                                        case 'f':
                                            rec.Sex = TGEDCOMSex.svFemale;
                                            break;
                                        case 'm':
                                            rec.Sex = TGEDCOMSex.svMale;
                                            break;
                                    }
                                }

                                this.fNames.Add(rec);
                            }
                        }
                    }
                }
            }

            using (MemoryStream memStream = new MemoryStream(GKResources.book_names_cf))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
			    {
				    while (strd.Peek() != -1) {
					    string ns = strd.ReadLine().Trim();
					    this.fChurchFNames.Add(ns);
				    }
			    }
            }

            using (MemoryStream memStream = new MemoryStream(GKResources.book_names_cm))
		    {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        this.fChurchMNames.Add(ns);
                    }
                }
            }
		}

        private void UpdateList()
        {
            this.cbNames.BeginUpdate();
            try
            {
                this.cbNames.Items.Clear();
                int num = this.fNames.Count - 1;
                for (int i = 0; i <= num; i++)
                {
                    TNameRecord rec = this.fNames[i];
                    string ns = rec.Name;
                    this.cbNames.Items.Add(new GKComboItem(ns, rec));

                    rec.ChIndex = -1;
                    ns = ns.ToUpper();

                    StringList lst = null;
                    if (rec.Sex == TGEDCOMSex.svMale)
                    {
                        lst = this.fChurchMNames;
                    }
                    else if (rec.Sex == TGEDCOMSex.svFemale)
                    {
                        lst = this.fChurchFNames;
                    }

                    int num2 = lst.Count - 1;
                    for (int j = 0; j <= num2; j++)
                    {
                        string st = lst[j];
                        if (st[0] == '-' && st.IndexOf(ns) >= 0)
                        {
                            rec.ChIndex = j;
                            break;
                        }
                    }
                }
            }
            finally
            {
                this.cbNames.EndUpdate();
            }
        }
	}
}
