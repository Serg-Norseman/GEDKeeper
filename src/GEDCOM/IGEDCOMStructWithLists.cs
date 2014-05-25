namespace GedCom551
{
    public interface IGEDCOMStructWithLists
    {
        GEDCOMList<TGEDCOMNotes> Notes { get; }
        GEDCOMList<TGEDCOMSourceCitation> SourceCitations { get; }
        GEDCOMList<TGEDCOMMultimediaLink> MultimediaLinks { get; }

        TGEDCOMNotes aux_AddNote(TGEDCOMNoteRecord noteRec);
        TGEDCOMSourceCitation aux_AddSource(TGEDCOMSourceRecord sourceRec, string page, int quality);
        TGEDCOMMultimediaLink aux_AddMultimedia(TGEDCOMMultimediaRecord mediaRec);
    }
}