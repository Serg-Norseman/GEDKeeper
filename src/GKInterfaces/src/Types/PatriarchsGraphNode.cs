namespace GKCore.Types
{
    public sealed class PatriarchsGraphNode
    {
        public string FamilyXRef;
        public NodeType Type;

        public PatriarchsGraphNode(string label, NodeType type)
        {
            this.FamilyXRef = label;
            this.Type = type;
        }
    }
}