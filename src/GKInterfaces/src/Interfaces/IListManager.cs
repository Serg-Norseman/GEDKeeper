using GKCommon.GEDCOM;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(GEDCOMRecord record);

	public interface IListManager
	{
		ExternalFilterHandler ExternalFilter { get; set; }
		IListFilter Filter { get; }
		IListColumns ListColumns { get; }
	}
}
