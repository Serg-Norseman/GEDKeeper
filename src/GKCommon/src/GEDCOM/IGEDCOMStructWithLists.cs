namespace GKCommon.GEDCOM
{
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