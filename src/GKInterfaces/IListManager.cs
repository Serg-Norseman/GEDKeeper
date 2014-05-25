using System;
using GedCom551;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(TGEDCOMRecord record);

	public interface IListManager
	{
		ExternalFilterHandler ExternalFilter { get; set; }
		IListFilter Filter { get; }
	}
}
