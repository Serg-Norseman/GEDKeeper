using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmSexCheck : Form
	{
		public TfmSexCheck()
		{
			this.InitializeComponent();
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[104];
			this.GroupBox1.Text = GKL.LSList[87];
			this.sbMale.Text = GKL.LSList[66];
			this.sbFemale.Text = GKL.LSList[67];
		}

		public static TGEDCOMSex DefineSex(string iName, string iPatr, TNamesTable aNamesTable)
		{
			TGEDCOMSex sx = aNamesTable.GetSexByName(iName);
			TGEDCOMSex Result = sx;
			if (sx == TGEDCOMSex.svNone)
			{
				TfmSexCheck dlg = new TfmSexCheck();
				try
				{
					if (dlg != null)
					{
						dlg.edName.Text = iName + " " + iPatr;
						sx = TGenEngine.GetSex(iName, iPatr, false);
						if (sx != TGEDCOMSex.svNone)
						{
							if (sx == TGEDCOMSex.svMale)
							{
								dlg.sbMale.Checked = true;
								goto IL_78;
							}
							if (sx == TGEDCOMSex.svFemale)
							{
								dlg.sbFemale.Checked = true;
								goto IL_78;
							}
							if (sx != TGEDCOMSex.svUndetermined)
							{
								goto IL_78;
							}
						}
						dlg.sbNone.Checked = true;
						IL_78:
						if (dlg.ShowDialog() == DialogResult.OK)
						{
							if (dlg.sbNone.Checked)
							{
								sx = TGEDCOMSex.svNone;
							}
							else
							{
								if (dlg.sbMale.Checked)
								{
									sx = TGEDCOMSex.svMale;
								}
								else
								{
									if (dlg.sbFemale.Checked)
									{
										sx = TGEDCOMSex.svFemale;
									}
								}
							}
							Result = sx;
							if (sx != TGEDCOMSex.svNone)
							{
								aNamesTable.SetNameSex(iName, sx);
							}
						}
					}
				}
				finally
				{
					TObjectHelper.Free(dlg);
				}
			}
			return Result;
		}

		public static void CheckPersonSex(TGEDCOMIndividualRecord iRec, TNamesTable aNamesTable)
		{
			if (iRec.Sex == TGEDCOMSex.svNone || iRec.Sex == TGEDCOMSex.svUndetermined)
			{
				string f_fam, f_name, f_patr;
				TGenEngine.GetNameParts(iRec, out f_fam, out f_name, out f_patr);
				iRec.Sex = DefineSex(f_name, f_patr, aNamesTable);
			}
		}

	}
}
