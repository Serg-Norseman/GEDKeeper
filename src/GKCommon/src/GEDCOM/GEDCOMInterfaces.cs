using System.Collections;

namespace GKCommon.GEDCOM
{
	public interface IGEDCOMListEnumerator : IEnumerator
	{
		GEDCOMObject Owner
		{
			get;
		}
	}

	public interface IGEDCOMTreeEnumerator
	{
		bool MoveNext(out GEDCOMRecord current);
		void Reset();
	}

    public interface IGEDCOMStructWithLists
    {
        GEDCOMList<GEDCOMNotes> Notes { get; }
        GEDCOMList<GEDCOMSourceCitation> SourceCitations { get; }
        GEDCOMList<GEDCOMMultimediaLink> MultimediaLinks { get; }

        GEDCOMNotes AddNote(GEDCOMNoteRecord noteRec);
        GEDCOMSourceCitation AddSource(GEDCOMSourceRecord sourceRec, string page, int quality);
        GEDCOMMultimediaLink AddMultimedia(GEDCOMMultimediaRecord mediaRec);
    }
}