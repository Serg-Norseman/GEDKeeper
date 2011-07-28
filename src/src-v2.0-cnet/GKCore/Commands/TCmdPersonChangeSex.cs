using GedCom551;
using GKCore;
using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore.Commands
{
	public class TCmdPersonChangeSex : TCustomCommand
	{
		internal string FPersonXRef;
		internal TGEDCOMObject.TGEDCOMSex FOldSex;
		internal TGEDCOMObject.TGEDCOMSex FNewSex;

		public TCmdPersonChangeSex(TUndoManager aManager, TGEDCOMIndividualRecord aPerson, TGEDCOMObject.TGEDCOMSex NewSex) : base(aManager)
		{
			this.FPersonXRef = aPerson.XRef;
			this.FOldSex = aPerson.Sex;
			this.FNewSex = NewSex;
		}

		public override bool Redo()
		{
			bool Result = true;
			TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)this.FManager.Tree.XRefIndex_Find(this.FPersonXRef);
			if (i_rec == null)
			{
				Result = false;
			}
			else
			{
				i_rec.Sex = this.FNewSex;
			}
			return Result;
		}

		public override void Undo()
		{
			TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)this.FManager.Tree.XRefIndex_Find(this.FPersonXRef);
			if (i_rec != null)
			{
				i_rec.Sex = this.FOldSex;
			}
		}
	}
}
