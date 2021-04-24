/**
 * DoubleMetaphone.cs
 * 
 * An implemenatation of Lawrence Phillips' Double Metaphone phonetic matching
 * algorithm, published in C/C++ Users Journal, June, 2000.
 * 
 * This implementation was written by Adam J. Nelson (anelson@nullpointer.net).
 * It is based on the C++ template implementation, also by Adam Nelson.
 * For the latest version of this implementation, implementations
 * in other languages, and links to articles I've written on the use of my various
 * Double Metaphone implementations, see:
 * http;//www.nullpointer.net/anelson/
 * 
 * Note that since this impl implements IComparable, it can be used to key associative containers,
 * thereby easily implementing phonetic matching within a simple container.  Examples of this
 * should have been included in the archive from which you obtained this file.
 * 
 * Current Version: 1.0.0
 * Revision History:
 * 	1.0.0 - ajn - First release
 * 
 * This implemenatation, and optimizations, Copyright (C) 2003, Adam J. Nelson
 * The Double Metaphone algorithm was written by Lawrence Phillips, and is
 * Copyright (c) 1998, 1999 by Lawrence Philips.
 */

using System;
using System.Text;

namespace GKStdReports
{
    /// <summary>Implements the Double Metaphone phonetic matching algorithm published
    ///     by Lawrence Phillips in June 2000 C/C++ Users Journal.
    /// 
    ///     Optimized and ported to C# by Adam Nelson (anelson@nullpointer.net)
    /// 										</summary>
    public class DoubleMetaphone
    {
        public const int METAPHONE_KEY_LENGTH = 4; //The length of the metaphone keys produced.  4 is sweet spot
        
        ///StringBuilders used to construct the keys
        private readonly StringBuilder m_primaryKey, m_alternateKey;
        
        ///Actual keys, populated after construction
        private string m_primaryKeyString, m_alternateKeyString;
        
        ///Variables to track the key length w/o having to grab the .Length attr
        private int m_primaryKeyLength, m_alternateKeyLength;
        
        ///Working copy of the word, and the orFamilySearchnal word
        private string m_word, m_orFamilySearchnalWord;
        
        ///Length and last valid zero-based index into word
        int m_length, m_last;
        
        ///Flag indicating if an alternate metaphone key was computed for the word
        bool m_hasAlternate;
        
        /// <summary>Default ctor, initializes by computing the keys of an empty string,
        ///     which are both empty strings</summary>
        public DoubleMetaphone()
        {
            //Leave room at the end for writing a bit beyond the length; keys are chopped at the end anyway
            m_primaryKey = new StringBuilder(METAPHONE_KEY_LENGTH+2);
            m_alternateKey = new StringBuilder(METAPHONE_KEY_LENGTH+2);
            
            ComputeKeys("");
        }
        
        /// <summary>Constructs a new DoubleMetaphone object, and initializes it with
        ///     the metaphone keys for a given word</summary>
        /// 
        /// <param name="word">Word with which to initialize the object.  Computes the metaphone keys
        ///     of this word.</param>
        public DoubleMetaphone(string word)
        {
            //Leave room at the end for writing a bit beyond the length; keys are chopped at the end anyway
            m_primaryKey = new StringBuilder(METAPHONE_KEY_LENGTH+2);
            m_alternateKey = new StringBuilder(METAPHONE_KEY_LENGTH+2);
            
            ComputeKeys(word);
        }
        
        /// <summary>The primary metaphone key for the current word</summary>
        public string PrimaryKey
        {
            get {
                return m_primaryKeyString;
            }
        }
        
        /// <summary>The alternate metaphone key for the current word, or null if the current
        ///     word does not have an alternate key by Double Metaphone</summary>
        public string AlternateKey
        {
            get {
                return m_hasAlternate ? m_alternateKeyString : null;
            }
        }
        
        /// <summary>OrFamilySearchnal word for which the keys were computed</summary>
        public string Word
        {
            get {
                return m_orFamilySearchnalWord;
            }
        }
        
        /// <summary>Static wrapper around the class, enables computation of metaphone keys
        ///     without instantiating a class.</summary>
        /// 
        /// <param name="word">Word whose metaphone keys are to be computed</param>
        /// <param name="primaryKey">Ref to var to receive primary metaphone key</param>
        /// <param name="alternateKey">Ref to var to receive alternate metaphone key, or be set to null if
        ///     word has no alternate key by double metaphone</param>
        public static void doubleMetaphone(string word, ref string primaryKey, ref string alternateKey)
        {
            DoubleMetaphone mp = new DoubleMetaphone(word);
            
            primaryKey = mp.PrimaryKey;
            alternateKey = mp.AlternateKey;
        }
        
        /// <summary>Sets a new current word for the instance, computing the new word's metaphone
        ///     keys</summary>
        /// 
        /// <param name="word">New word to set to current word.  Discards previous metaphone keys,
        ///     and computes new keys for this word</param>
        public virtual void ComputeKeys(string word)
        {
            m_primaryKey.Length = 0;
            m_alternateKey.Length = 0;
            
            m_primaryKeyString = "";
            m_alternateKeyString = "";
            
            m_primaryKeyLength = m_alternateKeyLength = 0;
            
            m_hasAlternate = false;
            
            m_orFamilySearchnalWord = word;
            
            //Copy word to an internal working buffer so it can be modified
            m_word = word;

            m_length = m_word.Length;

            //Compute last valid index into word
            m_last = m_length - 1;
            
            //Padd with four spaces, so word can be over-indexed without fear of exception
            m_word = String.Concat(m_word, "     ");
            
            //Convert to upper case, since metaphone is not case sensitive
            m_word = m_word.ToUpper();
            
            //Now build the keys
            BuildMetaphoneKeys();
        }
        
        /**
         * Internal impl of double metaphone algorithm.  Populates m_primaryKey and m_alternateKey.  Modified copy-past of
         * Phillips' orFamilySearchnal code
         */
        private void BuildMetaphoneKeys()
        {
            int current = 0;
            if (m_length < 1)
                return;
            
            //skip these when at start of word
            if (AreStringsAt(0, 2, "GN", "KN", "PN", "WR", "PS"))
                current += 1;
            
            //Initial 'X' is pronounced 'Z' e.g. 'Xavier'
            if (m_word[0] == 'X') {
                AddMetaphoneCharacter("S");	//'Z' maps to 'S'
                current += 1;
            }
            
            ///////////main loop//////////////////////////
            while ((m_primaryKeyLength < METAPHONE_KEY_LENGTH) || (m_alternateKeyLength < METAPHONE_KEY_LENGTH)) {
                if (current >= m_length)
                    break;
                
                switch (m_word[current]) {
                    case 'A':
                    case 'E':
                    case 'I':
                    case 'O':
                    case 'U':
                    case 'Y':
                        if (current == 0)
                            //all init vowels now map to 'A'
                            AddMetaphoneCharacter("A");
                        current +=1;
                        break;
                        
                    case 'B':
                        
                        //"-mb", e.g", "dumb", already skipped over...
                        AddMetaphoneCharacter("P");
                        
                        if (m_word[current + 1] == 'B')
                            current +=2;
                        else
                            current	+=1;
                        break;
                        
                    case 'З':
                        AddMetaphoneCharacter("S");
                        current += 1;
                        break;
                        
                    case 'C':
                        //various germanic
                        if ((current > 1)
                            && !IsVowel(current - 2)
                            && AreStringsAt((current - 1), 3, "ACH")
                            && ((m_word[current + 2] != 'I') && ((m_word[current + 2] != 'E')
                                                                 || AreStringsAt((current - 2), 6, "BACHER", "MACHER")) )) {
                            AddMetaphoneCharacter("K");
                            current +=2;
                            break;
                        }
                        
                        //special case 'caesar'
                        if ((current == 0) && AreStringsAt(current, 6, "CAESAR")) {
                            AddMetaphoneCharacter("S");
                            current +=2;
                            break;
                        }
                        
                        //italian 'chianti'
                        if (AreStringsAt(current, 4, "CHIA")) {
                            AddMetaphoneCharacter("K");
                            current +=2;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "CH")) {
                            //find 'michael'
                            if ((current > 0) && AreStringsAt(current, 4, "CHAE")) {
                                AddMetaphoneCharacter("K", "X");
                                current +=2;
                                break;
                            }
                            
                            //greek roots e.g. 'chemistry', 'chorus'
                            if ((current == 0)
                                && (AreStringsAt((current + 1), 5, "HARAC", "HARIS")
                                    || AreStringsAt((current + 1), 3, "HOR", "HYM", "HIA", "HEM"))
                                && !AreStringsAt(0, 5, "CHORE")) {
                                AddMetaphoneCharacter("K");
                                current +=2;
                                break;
                            }
                            
                            //germanic, greek, or otherwise 'ch' for 'kh' sound
                            if ((AreStringsAt(0, 4, "VAN ", "VON ") || AreStringsAt(0, 3, "SCH"))
                                // 'architect but not 'arch', 'orchestra', 'orchid'
                                || AreStringsAt((current - 2), 6, "ORCHES", "ARCHIT", "ORCHID")
                                || AreStringsAt((current + 2), 1, "T", "S")
                                || ((AreStringsAt((current - 1), 1, "A", "O", "U", "E") || (current == 0))
                                    //e.g., 'wachtler', 'wechsler', but not 'tichner'
                                    && AreStringsAt((current + 2), 1, "L", "R", "N", "M", "B", "H", "F", "V", "W", " "))) {
                                AddMetaphoneCharacter("K");
                            } else {
                                if (current > 0) {
                                    if (AreStringsAt(0, 2, "MC"))
                                        //e.g., "McHugh"
                                        AddMetaphoneCharacter("K");
                                    else
                                        AddMetaphoneCharacter("X", "K");
                                } else
                                    AddMetaphoneCharacter("X");
                            }
                            current +=2;
                            break;
                        }
                        //e.g, 'czerny'
                        if (AreStringsAt(current, 2, "CZ") && !AreStringsAt((current - 2), 4, "WICZ")) {
                            AddMetaphoneCharacter("S", "X");
                            current += 2;
                            break;
                        }
                        
                        //e.g., 'focaccia'
                        if (AreStringsAt((current + 1), 3, "CIA")) {
                            AddMetaphoneCharacter("X");
                            current += 3;
                            break;
                        }
                        
                        //double 'C', but not if e.g. 'McClellan'
                        if (AreStringsAt(current, 2, "CC") && !((current == 1) && (m_word[0] == 'M')))
                            //'bellocchio' but not 'bacchus'
                            if (AreStringsAt((current + 2), 1, "I", "E", "H") && !AreStringsAt((current + 2), 2, "HU")) {
                            //'accident', 'accede' 'succeed'
                            if (((current == 1) && (m_word[current - 1] == 'A'))
                                || AreStringsAt((current - 1), 5, "UCCEE", "UCCES"))
                                AddMetaphoneCharacter("KS");
                            //'bacci', 'bertucci', other italian
                            else
                                AddMetaphoneCharacter("X");
                            current += 3;
                            break;
                        } else {//Pierce's rule
                            AddMetaphoneCharacter("K");
                            current += 2;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "CK", "CG", "CQ")) {
                            AddMetaphoneCharacter("K");
                            current += 2;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "CI", "CE", "CY")) {
                            //italian vs. english
                            if (AreStringsAt(current, 3, "CIO", "CIE", "CIA"))
                                AddMetaphoneCharacter("S", "X");
                            else
                                AddMetaphoneCharacter("S");
                            current += 2;
                            break;
                        }
                        
                        //else
                        AddMetaphoneCharacter("K");
                        
                        //name sent in 'mac caffrey', 'mac gregor
                        if (AreStringsAt((current + 1), 2, " C", " Q", " G"))
                            current += 3;
                        else
                            if (AreStringsAt((current + 1), 1, "C", "K", "Q")
                                && !AreStringsAt((current + 1), 2, "CE", "CI"))
                                current += 2;
                            else
                                current	+= 1;
                        break;
                        
                    case 'D':
                        if (AreStringsAt(current, 2, "DG"))
                            if (AreStringsAt((current + 2), 1, "I", "E", "Y")) {
                            //e.g. 'edge'
                            AddMetaphoneCharacter("J");
                            current += 3;
                            break;
                        } else {
                            //e.g. 'edgar'
                            AddMetaphoneCharacter("TK");
                            current += 2;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "DT", "DD")) {
                            AddMetaphoneCharacter("T");
                            current += 2;
                            break;
                        }
                        
                        //else
                        AddMetaphoneCharacter("T");
                        current += 1;
                        break;
                        
                    case 'F':
                        if (m_word[current + 1] == 'F')
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("F");
                        break;
                        
                    case 'G':
                        if (m_word[current + 1] == 'H') {
                            if ((current > 0) && !IsVowel(current - 1)) {
                                AddMetaphoneCharacter("K");
                                current += 2;
                                break;
                            }
                            
                            if (current < 3) {
                                //'ghislane', ghiradelli
                                if (current == 0) {
                                    if (m_word[current + 2] == 'I')
                                        AddMetaphoneCharacter("J");
                                    else
                                        AddMetaphoneCharacter("K");
                                    current += 2;
                                    break;
                                }
                            }
                            //Parker's rule (with some further refinements) - e.g., 'hugh'
                            if (((current > 1) && AreStringsAt((current - 2), 1, "B", "H", "D") )
                                //e.g., 'bough'
                                || ((current > 2) && AreStringsAt((current - 3), 1, "B", "H", "D") )
                                //e.g., 'broughton'
                                || ((current > 3) && AreStringsAt((current - 4), 1, "B", "H") )) {
                                current += 2;
                                break;
                            } else {
                                //e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough'
                                if ((current > 2)
                                    && (m_word[current - 1] == 'U')
                                    && AreStringsAt((current - 3), 1, "C", "G", "L", "R", "T")) {
                                    AddMetaphoneCharacter("F");
                                } else
                                    if ((current > 0) && m_word[current - 1] != 'I')
                                        AddMetaphoneCharacter("K");
                                
                                current += 2;
                                break;
                            }
                        }
                        
                        if (m_word[current + 1] == 'N') {
                            if ((current == 1) && IsVowel(0) && !IsWordSlavoGermanic()) {
                                AddMetaphoneCharacter("KN", "N");
                            } else
                                //not e.g. 'cagney'
                                if (!AreStringsAt((current + 2), 2, "EY")
                                    && (m_word[current + 1] != 'Y') && !IsWordSlavoGermanic()) {
                                AddMetaphoneCharacter("N", "KN");
                            } else
                                AddMetaphoneCharacter("KN");
                            current += 2;
                            break;
                        }
                        
                        //'tagliaro'
                        if (AreStringsAt((current + 1), 2, "LI") && !IsWordSlavoGermanic()) {
                            AddMetaphoneCharacter("KL", "L");
                            current += 2;
                            break;
                        }
                        
                        //-ges-,-gep-,-gel-, -gie- at beginning
                        if ((current == 0)
                            && ((m_word[current + 1] == 'Y')
                                || AreStringsAt((current + 1), 2, "ES", "EP", "EB", "EL", "EY", "IB", "IL", "IN", "IE", "EI", "ER"))) {
                            AddMetaphoneCharacter("K", "J");
                            current += 2;
                            break;
                        }
                        
                        // -ger-,  -gy-
                        if ((AreStringsAt((current + 1), 2, "ER") || (m_word[current + 1] == 'Y'))
                            && !AreStringsAt(0, 6, "DANGER", "RANGER", "MANGER")
                            && !AreStringsAt((current - 1), 1, "E", "I")
                            && !AreStringsAt((current - 1), 3, "RGY", "OGY")) {
                            AddMetaphoneCharacter("K", "J");
                            current += 2;
                            break;
                        }
                        
                        // italian e.g, 'biaggi'
                        if (AreStringsAt((current + 1), 1, "E", "I", "Y") || AreStringsAt((current - 1), 4, "AGGI", "OGGI")) {
                            //obvious germanic
                            if ((AreStringsAt(0, 4, "VAN ", "VON ") || AreStringsAt(0, 3, "SCH"))
                                || AreStringsAt((current + 1), 2, "ET"))
                                AddMetaphoneCharacter("K");
                            else
                                //always soft if french ending
                                if (AreStringsAt((current + 1), 4, "IER "))
                                    AddMetaphoneCharacter("J");
                                else
                                    AddMetaphoneCharacter("J", "K");
                            current += 2;
                            break;
                        }
                        
                        if (m_word[current + 1] == 'G')
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("K");
                        break;
                        
                    case 'H':
                        //only keep if first & before vowel or btw. 2 vowels
                        if (((current == 0) || IsVowel(current - 1))
                            && IsVowel(current + 1)) {
                            AddMetaphoneCharacter("H");
                            current += 2;
                        } else//also takes care of 'HH'
                            current	+= 1;
                        break;
                        
                    case 'J':
                        //obvious spanish, 'jose', 'san jacinto'
                        if (AreStringsAt(current, 4, "JOSE") || AreStringsAt(0, 4, "SAN ")) {
                            if (((current == 0) && (m_word[current + 4] == ' ')) || AreStringsAt(0, 4, "SAN "))
                                AddMetaphoneCharacter("H");
                            else {
                                AddMetaphoneCharacter("J", "H");
                            }
                            current +=1;
                            break;
                        }
                        
                        if ((current == 0) && !AreStringsAt(current, 4, "JOSE"))
                            AddMetaphoneCharacter("J", "A"); //Yankelovich/Jankelowicz
                        else
                            //spanish pron. of e.g. 'bajador'
                            if (IsVowel(current - 1)
                                && !IsWordSlavoGermanic()
                                && ((m_word[current + 1] == 'A') || (m_word[current + 1] == 'O')))
                                AddMetaphoneCharacter("J", "H");
                            else
                                if (current == m_last)
                                    AddMetaphoneCharacter("J", " ");
                                else
                                    if (!AreStringsAt((current + 1), 1, "L", "T", "K", "S", "N", "M", "B", "Z")
                                        && !AreStringsAt((current - 1), 1, "S", "K", "L"))
                                        AddMetaphoneCharacter("J");
                        
                        if (m_word[current + 1] == 'J') //it could happen!
                            current += 2;
                        else
                            current	+= 1;
                        break;
                        
                    case 'K':
                        if (m_word[current + 1] == 'K')
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("K");
                        break;
                        
                    case 'L':
                        if (m_word[current + 1] == 'L') {
                            //spanish e.g. 'cabrillo', 'gallegos'
                            if (((current == (m_length - 3))
                                 && AreStringsAt((current - 1), 4, "ILLO", "ILLA", "ALLE"))
                                || ((AreStringsAt((m_last - 1), 2, "AS", "OS") || AreStringsAt(m_last, 1, "A", "O"))
                                    && AreStringsAt((current - 1), 4, "ALLE"))) {
                                AddMetaphoneCharacter("L", " ");
                                current += 2;
                                break;
                            }
                            current += 2;
                        } else
                            current	+= 1;
                        AddMetaphoneCharacter("L");
                        break;
                        
                    case 'M':
                        if ((AreStringsAt((current - 1), 3, "UMB")
                             && (((current + 1) == m_last) || AreStringsAt((current + 2), 2, "ER")))
                            //'dumb','thumb'
                            ||  (m_word[current + 1] == 'M'))
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("M");
                        break;
                        
                    case 'N':
                        if (m_word[current + 1] == 'N')
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("N");
                        break;
                        
                    case 'С':
                        current += 1;
                        AddMetaphoneCharacter("N");
                        break;
                        
                    case 'P':
                        if (m_word[current + 1] == 'H') {
                            AddMetaphoneCharacter("F");
                            current += 2;
                            break;
                        }
                        
                        //also account for "campbell", "raspberry"
                        if (AreStringsAt((current + 1), 1, "P", "B"))
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("P");
                        break;
                        
                    case 'Q':
                        if (m_word[current + 1] == 'Q')
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("K");
                        break;
                        
                    case 'R':
                        //french e.g. 'rogier', but exclude 'hochmeier'
                        if ((current == m_last)
                            && !IsWordSlavoGermanic()
                            && AreStringsAt((current - 2), 2, "IE")
                            && !AreStringsAt((current - 4), 2, "ME", "MA"))
                            AddMetaphoneCharacter("", "R");
                        else
                            AddMetaphoneCharacter("R");
                        
                        if (m_word[current + 1] == 'R')
                            current += 2;
                        else
                            current	+= 1;
                        break;
                        
                    case 'S':
                        //special cases 'island', 'isle', 'carlisle', 'carlysle'
                        if (AreStringsAt((current - 1), 3, "ISL", "YSL")) {
                            current += 1;
                            break;
                        }
                        
                        //special case 'sugar-'
                        if ((current == 0) && AreStringsAt(current, 5, "SUGAR")) {
                            AddMetaphoneCharacter("X", "S");
                            current += 1;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "SH")) {
                            //germanic
                            if (AreStringsAt((current + 1), 4, "HEIM", "HOEK", "HOLM", "HOLZ"))
                                AddMetaphoneCharacter("S");
                            else
                                AddMetaphoneCharacter("X");
                            current += 2;
                            break;
                        }
                        
                        //italian & armenian
                        if (AreStringsAt(current, 3, "SIO", "SIA") || AreStringsAt(current, 4, "SIAN")) {
                            if (!IsWordSlavoGermanic())
                                AddMetaphoneCharacter("S", "X");
                            else
                                AddMetaphoneCharacter("S");
                            current += 3;
                            break;
                        }
                        
                        //german & anglicisations, e.g. 'smith' match 'schmidt', 'snider' match 'schneider'
                        //also, -sz- in slavic language altho in hungarian it is pronounced 's'
                        if (((current == 0)
                             && AreStringsAt((current + 1), 1, "M", "N", "L", "W"))
                            || AreStringsAt((current + 1), 1, "Z")) {
                            AddMetaphoneCharacter("S", "X");
                            if (AreStringsAt((current + 1), 1, "Z"))
                                current += 2;
                            else
                                current	+= 1;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "SC")) {
                            //Schlesinger's rule
                            if (m_word[current + 2] == 'H')
                                //dutch orFamilySearchn, e.g. 'school', 'schooner'
                                if (AreStringsAt((current + 3), 2, "OO", "ER", "EN", "UY", "ED", "EM")) {
                                //'schermerhorn', 'schenker'
                                if (AreStringsAt((current + 3), 2, "ER", "EN")) {
                                    AddMetaphoneCharacter("X", "SK");
                                } else
                                    AddMetaphoneCharacter("SK");
                                current += 3;
                                break;
                            } else {
                                if ((current == 0) && !IsVowel(3) && (m_word[3] != 'W'))
                                    AddMetaphoneCharacter("X", "S");
                                else
                                    AddMetaphoneCharacter("X");
                                current += 3;
                                break;
                            }
                            
                            if (AreStringsAt((current + 2), 1, "I", "E", "Y")) {
                                AddMetaphoneCharacter("S");
                                current += 3;
                                break;
                            }
                            //else
                            AddMetaphoneCharacter("SK");
                            current += 3;
                            break;
                        }
                        
                        //french e.g. 'resnais', 'artois'
                        if ((current == m_last) && AreStringsAt((current - 2), 2, "AI", "OI"))
                            AddMetaphoneCharacter("", "S");
                        else
                            AddMetaphoneCharacter("S");
                        
                        if (AreStringsAt((current + 1), 1, "S", "Z"))
                            current += 2;
                        else
                            current	+= 1;
                        break;
                        
                    case 'T':
                        if (AreStringsAt(current, 4, "TION")) {
                            AddMetaphoneCharacter("X");
                            current += 3;
                            break;
                        }
                        
                        if (AreStringsAt(current, 3, "TIA", "TCH")) {
                            AddMetaphoneCharacter("X");
                            current += 3;
                            break;
                        }
                        
                        if (AreStringsAt(current, 2, "TH")
                            || AreStringsAt(current, 3, "TTH")) {
                            //special case 'thomas', 'thames' or germanic
                            if (AreStringsAt((current + 2), 2, "OM", "AM")
                                || AreStringsAt(0, 4, "VAN ", "VON ")
                                || AreStringsAt(0, 3, "SCH")) {
                                AddMetaphoneCharacter("T");
                            } else {
                                AddMetaphoneCharacter("0", "T");
                            }
                            current += 2;
                            break;
                        }
                        
                        if (AreStringsAt((current + 1), 1, "T", "D"))
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("T");
                        break;
                        
                    case 'V':
                        if (m_word[current + 1] == 'V')
                            current += 2;
                        else
                            current	+= 1;
                        AddMetaphoneCharacter("F");
                        break;
                        
                    case 'W':
                        //can also be in middle of word
                        if (AreStringsAt(current, 2, "WR")) {
                            AddMetaphoneCharacter("R");
                            current += 2;
                            break;
                        }
                        
                        if ((current == 0)
                            && (IsVowel(current + 1) || AreStringsAt(current, 2, "WH"))) {
                            //Wasserman should match Vasserman
                            if (IsVowel(current + 1))
                                AddMetaphoneCharacter("A", "F");
                            else
                                //need Uomo to match Womo
                                AddMetaphoneCharacter("A");
                        }
                        
                        //Arnow should match Arnoff
                        if (((current == m_last) && IsVowel(current - 1))
                            || AreStringsAt((current - 1), 5, "EWSKI", "EWSKY", "OWSKI", "OWSKY")
                            || AreStringsAt(0, 3, "SCH")) {
                            AddMetaphoneCharacter("", "F");
                            current +=1;
                            break;
                        }
                        
                        //polish e.g. 'filipowicz'
                        if (AreStringsAt(current, 4, "WICZ", "WITZ")) {
                            AddMetaphoneCharacter("TS", "FX");
                            current +=4;
                            break;
                        }
                        
                        //else skip it
                        current +=1;
                        break;
                        
                    case 'X':
                        //french e.g. breaux
                        if (!((current == m_last)
                              && (AreStringsAt((current - 3), 3, "IAU", "EAU")
                                  || AreStringsAt((current - 2), 2, "AU", "OU"))))
                            AddMetaphoneCharacter("KS");
                        
                        if (AreStringsAt((current + 1), 1, "C", "X"))
                            current += 2;
                        else
                            current	+= 1;
                        break;
                        
                    case 'Z':
                        //chinese pinyin e.g. 'zhao'
                        if (m_word[current + 1] == 'H') {
                            AddMetaphoneCharacter("J");
                            current += 2;
                            break;
                        } else
                            if (AreStringsAt((current + 1), 2, "ZO", "ZI", "ZA")
                                || (IsWordSlavoGermanic() && ((current > 0) && m_word[current - 1] != 'T'))) {
                            AddMetaphoneCharacter("S", "TS");
                        } else
                            AddMetaphoneCharacter("S");
                        
                        if (m_word[current + 1] == 'Z')
                            current += 2;
                        else
                            current	+= 1;
                        break;
                        
                    default:
                        current += 1;
                        break;
                }
            }
            
            //Finally, chop off the keys at the proscribed length
            if (m_primaryKeyLength > METAPHONE_KEY_LENGTH) {
                m_primaryKey.Length = METAPHONE_KEY_LENGTH;
            }
            
            if (m_alternateKeyLength > METAPHONE_KEY_LENGTH) {
                m_alternateKey.Length = METAPHONE_KEY_LENGTH;
            }
            
            m_primaryKeyString = m_primaryKey.ToString();
            m_alternateKeyString = m_alternateKey.ToString();
        }
        
        /**
         * Returns true if m_word is classified as "slavo-germanic" by Phillips' algorithm
         * 
         * @return true if word contains strings that Lawrence's algorithm considers indicative of
         *         slavo-germanic orFamilySearchn; else false
         */
        private bool IsWordSlavoGermanic()
        {
            if((m_word.IndexOf("W") != -1) ||
               (m_word.IndexOf("K") != -1) ||
               (m_word.IndexOf("CZ") != -1) ||
               (m_word.IndexOf("WITZ") != -1))
                return true;
            
            return false;
        }
        
        /**
         * Returns true if letter at given position in word is a Roman vowel
         * 
         * @param pos    Position at which to check for a vowel
         * 
         * @return True if m_word[pos] is a Roman vowel, else false
         */
        private bool IsVowel(int pos)
        {
            if ((pos < 0) || (pos >= m_length))
                return false;
            
            Char it = m_word[pos];
            
            if ((it == 'E') || (it == 'A') || (it == 'I') || (it == 'O') || (it == 'U') || (it == 'Y'))
                return true;
            
            return false;
        }
        
        /**
         * Appends the given metaphone character to the primary and alternate keys
         * 
         * @param primaryCharacter
         *               Character to append
         */
        private void AddMetaphoneCharacter(string primaryCharacter)
        {
            AddMetaphoneCharacter(primaryCharacter, null);
        }
        
        /**
         * Appends a metaphone character to the primary, and a possibly different alternate,
         * metaphone keys for the word.
         * 
         * @param primaryCharacter
         *               Primary character to append to primary key, and, if no alternate char is present,
         *               the alternate key as well
         * @param alternateCharacter
         *               Alternate character to append to alternate key.  May be null or a zero-length string,
         *               in which case the primary character will be appended to the alternate key instead
         */
        private void AddMetaphoneCharacter(string primaryCharacter, string alternateCharacter)
        {
            //Is the primary character valid?
            if (primaryCharacter.Length > 0) {
                int idx = 0;
                while (idx < primaryCharacter.Length) {
                    m_primaryKey.Length++;
                    m_primaryKey[m_primaryKeyLength++] = primaryCharacter[idx++];
                }
            }
            
            //Is the alternate character valid?
            if (alternateCharacter != null) {
                //Alternate character was provided.  If it is not zero-length, append it, else
                //append the primary string as long as it wasn't zero length and isn't a space character
                if (alternateCharacter.Length > 0) {
                    m_hasAlternate = true;
                    if (alternateCharacter[0] != ' ') {
                        int idx = 0;
                        while (idx < alternateCharacter.Length) {
                            m_alternateKey.Length++;
                            m_alternateKey[m_alternateKeyLength++] = alternateCharacter[idx++];
                        }
                    }
                } else {
                    //No, but if the primary character is valid, add that instead
                    if (primaryCharacter.Length > 0 && (primaryCharacter[0] != ' ')) {
                        int idx = 0;
                        while (idx < primaryCharacter.Length) {
                            m_alternateKey.Length++;
                            m_alternateKey[m_alternateKeyLength++] = primaryCharacter[idx++];
                        }
                    }
                }
            } else if (primaryCharacter.Length > 0) {
                //Else, no alternate character was passed, but a primary was, so append the primary character to the alternate key
                int idx = 0;
                while (idx < primaryCharacter.Length) {
                    m_alternateKey.Length++;
                    m_alternateKey[m_alternateKeyLength++] = primaryCharacter[idx++];
                }
            }
        }
        
        /**
         * Tests if any of the strings passed as variable arguments are at the given start position and
         * length within word
         * 
         * @param start   Start position in m_word
         * @param length  Length of substring starting at start in m_word to compare to the given strings
         * @param strings params array of zero or more strings for which to search in m_word
         * 
         * @return true if any one string in the strings array was found in m_word at the given position
         *         and length
         */
        private bool AreStringsAt(int start, int length, params String[] strings)
        {
            if (start < 0)
            {
                //Sometimes, as a result of expressions like "current - 2" for start,
                //start ends up negative.  Since no string can be present at a negative offset, this is always false
                return false;
            }
            
            string target = m_word.Substring(start, length);
            
            for (int idx = 0; idx < strings.Length; idx++) {
                if (strings[idx] == target) {
                    return true;
                }
            }
            
            return false;
        }
    }
}
