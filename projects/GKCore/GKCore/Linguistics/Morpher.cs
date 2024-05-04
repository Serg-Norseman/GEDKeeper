/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2015-2024 by Serg V. Zhdanovskih.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Runtime.CompilerServices;
using System.Text;

namespace GKCore.Linguistics
{
    public enum TranslitScheme
    {
        // Russian
        ts_Russian,
        // Library of Congress
        ts_LibCongress,
        // Pokrovsky EuroTex-92
        ts_Pokrovsky,
        // Volapuyk
        ts_Volapuyk,
        // GOST 16876-71
        ts_GOST,
        // Simplified
        ts_Simplified
    }

    public enum Declension
    {
        dUndefined,
        d1,
        d2,
        d3,
        d4
    }

    public enum Number
    {
        nUndefined,
        nSingle,
        nPlural
    }

    /// <summary>
    /// Род
    /// </summary>
    public enum DeclensionGender
    {
        /// <summary>
        /// Род неопределен
        /// </summary>
        None = 0,

        /// <summary>
        /// Мужской род
        /// </summary>
        Masculine = 1,

        /// <summary>
        /// Женский род
        /// </summary>
        Feminine = 2,

        /// <summary>
        /// Нейтральный род
        /// </summary>
        Neutral = 3
    }

    /// <summary>
    /// Падежи русского языка
    /// </summary>
    public enum DeclensionCase
    {
        /// <summary>
        /// Падеж не определен
        /// </summary>
        None = 0,

        /// <summary>
        /// Именительный падеж (Кто? Что?)
        /// </summary>
        Nominative = 1,

        /// <summary>
        /// Родительный падеж (Кого? Чего?)
        /// </summary>
        Genitive = 2,

        /// <summary>
        /// Дательный падеж (Кому? Чему?)
        /// </summary>
        Dative = 3,

        /// <summary>
        /// Винительный падеж (Кого? Что?)
        /// </summary>
        Accusative = 4,

        /// <summary>
        /// Творительный падеж (Кем? Чем?)
        /// </summary>
        Instrumental = 5,

        /// <summary>
        /// Предложный падеж (О ком? О чём?)
        /// </summary>
        Prepositional = 6
    }

    /// <summary>
    /// 
    /// </summary>
    public enum PersonalPronoun
    {
        Surname, Name, Patronymic
    }

    /// <summary>
    /// 
    /// </summary>
    public static class Morpher
    {
        private const string SWordTooShort = "Word too short";
        private const string SBaseTooShort = "Word base too short";
        private const string vowels = "АаЕеЁёИиЙйОоУуЮюЫыЭэЯяEeUuIiOoAaJj";
        private const string consonants = "БбВвГгДдЖжЗзКкЛлМмНнПпРрСсТтЦцШшЩщФфХхЧчBbCcDdFfGgHhKkLlMmNnPpQqRrSsTtVvWwXxYyZz";
        private const string mixedconsonants = "гкхчшщж";

        private static readonly string[, , , ] NounEndings;
        private static readonly string[, , , ] AdjectiveEndings;

        private static char endof(string word)
        {
            return word[((word != null) ? word.Length : 0) - 1];
        }

        private static bool ends_in_one_of(string word, string letters)
        {
            int num = (letters != null) ? letters.Length : 0;
            for (int i = 0; i < num; i++) {
                if (endof(word) == letters[i]) {
                    return true;
                }
            }
            return false;
        }

        private static bool isConsonant(char c)
        {
            return consonants.IndexOf(c) >= 0;
        }

        private static bool isVowel(char c)
        {
            return vowels.IndexOf(c) >= 0;
        }

        public static string MorphNoun(string noun, DeclensionCase ncase, Number num, DeclensionGender gender, bool animate, bool endingstressed)
        {
            if (string.IsNullOrEmpty(noun))
                throw new ArgumentNullException("noun");

            if (noun.Length < 2) {
                throw new Exception(SWordTooShort);
            }

            char e = noun[noun.Length - 1];
            string _base;
            bool jot;
            Declension decl;
            bool soft;
            string result;

            switch (e) {
                case 'а':
                    _base = noun.Substring(0, noun.Length - 1);
                    jot = false;
                    decl = Declension.d1;
                    soft = false;
                    break;
                case 'я':
                    _base = noun.Substring(0, noun.Length - 1);
                    jot = (isVowel(endof(_base)) || endof(_base) == 'ь');
                    decl = Declension.d1;
                    soft = true;
                    break;
                case 'о':
                    _base = noun.Substring(0, noun.Length - 1);
                    jot = false;
                    decl = Declension.d3;
                    soft = false;
                    break;
                case 'е':
                case 'ё':
                    _base = noun.Substring(0, noun.Length - 1);
                    jot = (isVowel(endof(_base)) || endof(_base) == 'ь');
                    decl = Declension.d3;
                    soft = true;
                    break;
                case 'й':
                    _base = noun.Substring(0, noun.Length - 1);
                    jot = true;
                    decl = Declension.d2;
                    soft = true;
                    break;
                case 'ь':
                    _base = noun.Substring(0, noun.Length - 1);
                    jot = false;
                    decl = Declension.d4;
                    soft = false;
                    break;
                default:
                    if (isConsonant(e)) {
                        _base = noun;
                        jot = false;
                        decl = Declension.d2;
                        soft = false;
                    } else {
                        result = noun;
                        return result;
                    }
                    break;
            }

            if (((_base != null) ? _base.Length : 0) < 2) {
                throw new Exception(SBaseTooShort);
            }

            if (animate && ncase == DeclensionCase.Accusative && ((decl == Declension.d1 && num == Number.nPlural) || decl == Declension.d2)) {
                ncase = DeclensionCase.Genitive;
            }

            string ending = NounEndings[(int)decl - 1, (soft ? 1 : 0), (int)num - 1, (int)ncase - 1];

            if (gender == DeclensionGender.Neutral && num == Number.nPlural && ncase == DeclensionCase.Genitive) {
                ending = "";
            }

            if (num == Number.nSingle && ncase == DeclensionCase.Prepositional && jot && endof(_base) == 'и') {
                ending = "и";
            }

            if (ends_in_one_of(_base, mixedconsonants) && ending.CompareTo("ы") == 0) {
                ending = "и";
            }

            if (decl == Declension.d1) {
                if (((ending != null) ? ending.Length : 0) == 0 && jot) {
                    _base += "й";
                }
                if (num == Number.nSingle) {
                    if (ncase == DeclensionCase.Instrumental) {
                        if (ends_in_one_of(_base, "жшщчц")) {
                            if (endingstressed) {
                                ending = "ой";
                            } else {
                                ending = "ей";
                            }
                        } else {
                            if (soft && endingstressed) {
                                ending = "ёй";
                            }
                        }
                    }
                    if ((ncase == DeclensionCase.Dative && jot) && endof(_base) == 'и') {
                        ending = "и";
                    }
                } else {
                    if (ncase == DeclensionCase.Genitive) {
                        if (_base[((_base != null) ? _base.Length : 0) - 1] == 'ь') {
                            _base = _base.Substring(0, _base.Length - 1) + _base.Substring(_base.Length);
                        }
                        char c2 = _base[((_base != null) ? _base.Length : 0) - 1 - 1];
                        char c3 = _base[((_base != null) ? _base.Length : 0) - 1];
                        bool harden = false;
                        if ((isConsonant(c2) || c2 == 'ь') && isConsonant(c3)) {
                            char vowel = '\0';
                            if (_base.CompareTo("кочерг") == 0) {
                                vowel = 'ё';
                            } else {
                                if (soft) {
                                    if (jot && endingstressed) {
                                        vowel = 'е';
                                    } else {
                                        vowel = 'и';
                                    }
                                    if (c3 == 'н') {
                                        harden = (_base.CompareTo("барышн") != 0 && _base.CompareTo("боярышн") != 0 && _base.CompareTo("деревн") != 0);
                                    }
                                } else {
                                    if (c2 == 'ь') {
                                        vowel = 'е';
                                    } else {
                                        if (c3 == 'к') {
                                            if (c2 == 'й') {
                                                vowel = 'е';
                                            } else {
                                                vowel = 'о';
                                            }
                                        }
                                    }
                                }
                            }

                            if (vowel != '\0') {
                                if (c2 == 'ь' || c2 == 'й') {
                                    StringBuilder sb = new StringBuilder(_base);
                                    sb[_base.Length - 2] = vowel;
                                    _base = sb.ToString();
                                } else {
                                    StringBuilder sb = new StringBuilder(_base);
                                    sb.Insert(_base.Length - 1, vowel);
                                    _base = sb.ToString();
                                }
                            }
                        }
                        if (soft && !jot && !harden) {
                            _base += "ь";
                        }
                    }
                }
            } else {
                if (decl == Declension.d2) {
                    if (ncase == DeclensionCase.Accusative) {
                        ncase = DeclensionCase.Nominative;
                    }
                    if (num == Number.nSingle && ncase == DeclensionCase.Nominative) {
                        if (e == 'е') {
                            ending = "е";
                        }
                        if (e == 'о') {
                            ending = "о";
                        }
                    }
                    if (((ending != null) ? ending.Length : 0) == 0 && jot) {
                        _base += "й";
                    }
                    if (gender == DeclensionGender.Neutral && num == Number.nPlural && ncase == DeclensionCase.Nominative) {
                        if (soft) {
                            ending = "я";
                        } else {
                            ending = "а";
                        }
                    }
                }
            }
            result = _base + ending;
            return result;
        }

        public static string MorphAdjective(String adjective, DeclensionCase c, Number q, DeclensionGender g)
        {
            if (string.IsNullOrEmpty(adjective))
                throw new ArgumentNullException("adjective");

            if (adjective.Length < 4) {
                throw new Exception(SWordTooShort);
            }

            char e2 = adjective[adjective.Length - 1];
            char e = adjective[adjective.Length - 2];

            string _base;
            bool soft;
            if (e == 'ы' && e2 == 'й') {
                _base = adjective.Substring(0, adjective.Length - 2);
                soft = false;
            } else {
                if (e == 'и' && e2 == 'й') {
                    _base = adjective.Substring(0, adjective.Length - 2);
                    soft = true;
                } else {
                    if (e == 'о' && e2 == 'й') {
                        _base = adjective.Substring(0, adjective.Length - 2);
                        soft = true;
                    } else {
                        if (e == 'а' && e2 == 'я') {
                            _base = adjective.Substring(0, adjective.Length - 2);
                            soft = false;
                        } else {
                            if (e == 'я' && e2 == 'я') {
                                _base = adjective.Substring(0, adjective.Length - 2);
                                soft = true;
                            } else {
                                if (e == 'о' && e2 == 'е') {
                                    _base = adjective.Substring(0, adjective.Length - 2);
                                    soft = false;
                                } else {
                                    if (e == 'е' && e2 == 'е') {
                                        _base = adjective.Substring(0, adjective.Length - 2);
                                        soft = true;
                                    } else {
                                        if (!isConsonant(e)) {
                                            return adjective;
                                        }
                                        _base = adjective;
                                        soft = false;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (((_base != null) ? _base.Length : 0) < 2) {
                throw new Exception(SBaseTooShort);
            }

            string ending = AdjectiveEndings[(int)g - 1, (int)(soft ? 1 : 0), (int)q - 1, (int)c - 1];
            return _base + ending;
        }

        static Morpher()
        {
            string[,,,] array = new string[4, 2, 2, 6];
            array[0, 0, 0, 0] = "а";
            array[0, 0, 0, 1] = "ы";
            array[0, 0, 0, 2] = "е";
            array[0, 0, 0, 3] = "у";
            array[0, 0, 0, 4] = "ой";
            array[0, 0, 0, 5] = "е";
            array[0, 0, 1, 0] = "ы";
            array[0, 0, 1, 1] = "";
            array[0, 0, 1, 2] = "ам";
            array[0, 0, 1, 3] = "";
            array[0, 0, 1, 4] = "ами";
            array[0, 0, 1, 5] = "ах";
            array[0, 1, 0, 0] = "я";
            array[0, 1, 0, 1] = "и";
            array[0, 1, 0, 2] = "е";
            array[0, 1, 0, 3] = "ю";
            array[0, 1, 0, 4] = "ей";
            array[0, 1, 0, 5] = "е";
            array[0, 1, 1, 0] = "и";
            array[0, 1, 1, 1] = "ей";
            array[0, 1, 1, 2] = "ям";
            array[0, 1, 1, 3] = "ей";
            array[0, 1, 1, 4] = "ями";
            array[0, 1, 1, 5] = "ях";
            array[1, 0, 0, 0] = "";
            array[1, 0, 0, 1] = "а";
            array[1, 0, 0, 2] = "у";
            array[1, 0, 0, 3] = "а";
            array[1, 0, 0, 4] = "ом";
            array[1, 0, 0, 5] = "е";
            array[1, 0, 1, 0] = "ы";
            array[1, 0, 1, 1] = "ов";
            array[1, 0, 1, 2] = "ам";
            array[1, 0, 1, 3] = "ов";
            array[1, 0, 1, 4] = "ами";
            array[1, 0, 1, 5] = "ах";
            array[1, 1, 0, 0] = "ь";
            array[1, 1, 0, 1] = "я";
            array[1, 1, 0, 2] = "ю";
            array[1, 1, 0, 3] = "я";
            array[1, 1, 0, 4] = "ем";
            array[1, 1, 0, 5] = "е";
            array[1, 1, 1, 0] = "и";
            array[1, 1, 1, 1] = "ей";
            array[1, 1, 1, 2] = "ям";
            array[1, 1, 1, 3] = "ей";
            array[1, 1, 1, 4] = "ями";
            array[1, 1, 1, 5] = "ях";
            array[2, 0, 0, 0] = "о";
            array[2, 0, 0, 1] = "а";
            array[2, 0, 0, 2] = "у";
            array[2, 0, 0, 3] = "о";
            array[2, 0, 0, 4] = "ом";
            array[2, 0, 0, 5] = "е";
            array[2, 0, 1, 0] = "а";
            array[2, 0, 1, 1] = "";
            array[2, 0, 1, 2] = "ам";
            array[2, 0, 1, 3] = "а";
            array[2, 0, 1, 4] = "ами";
            array[2, 0, 1, 5] = "ах";
            array[2, 1, 0, 0] = "е";
            array[2, 1, 0, 1] = "я";
            array[2, 1, 0, 2] = "ю";
            array[2, 1, 0, 3] = "е";
            array[2, 1, 0, 4] = "ем";
            array[2, 1, 0, 5] = "е";
            array[2, 1, 1, 0] = "я";
            array[2, 1, 1, 1] = "ей";
            array[2, 1, 1, 2] = "ям";
            array[2, 1, 1, 3] = "я";
            array[2, 1, 1, 4] = "ями";
            array[2, 1, 1, 5] = "ях";
            array[3, 0, 0, 0] = "ь";
            array[3, 0, 0, 1] = "и";
            array[3, 0, 0, 2] = "и";
            array[3, 0, 0, 3] = "ь";
            array[3, 0, 0, 4] = "ью";
            array[3, 0, 0, 5] = "и";
            array[3, 0, 1, 0] = "и";
            array[3, 0, 1, 1] = "ей";
            array[3, 0, 1, 2] = "ам";
            array[3, 0, 1, 3] = "ей";
            array[3, 0, 1, 4] = "ами";
            array[3, 0, 1, 5] = "ах";
            array[3, 1, 0, 0] = "ь";
            array[3, 1, 0, 1] = "и";
            array[3, 1, 0, 2] = "и";
            array[3, 1, 0, 3] = "ь";
            array[3, 1, 0, 4] = "ью";
            array[3, 1, 0, 5] = "и";
            array[3, 1, 1, 0] = "и";
            array[3, 1, 1, 1] = "ей";
            array[3, 1, 1, 2] = "ям";
            array[3, 1, 1, 3] = "ей";
            array[3, 1, 1, 4] = "ями";
            array[3, 1, 1, 5] = "ях";
            NounEndings = array;


            string[, , , ] array2 = new string[3, 3, 2, 6];
            array2[0, 0, 0, 0] = "ый";
            array2[0, 0, 0, 1] = "ого";
            array2[0, 0, 0, 2] = "ому";
            array2[0, 0, 0, 3] = "ого";
            array2[0, 0, 0, 4] = "ым";
            array2[0, 0, 0, 5] = "ом";
            array2[0, 0, 1, 0] = "ые";
            array2[0, 0, 1, 1] = "ых";
            array2[0, 0, 1, 2] = "ым";
            array2[0, 0, 1, 3] = "ых";
            array2[0, 0, 1, 4] = "ыми";
            array2[0, 0, 1, 5] = "ых";
            array2[0, 1, 0, 0] = "ий";
            array2[0, 1, 0, 1] = "его";
            array2[0, 1, 0, 2] = "ему";
            array2[0, 1, 0, 3] = "его";
            array2[0, 1, 0, 4] = "им";
            array2[0, 1, 0, 5] = "ем";
            array2[0, 1, 1, 0] = "ие";
            array2[0, 1, 1, 1] = "их";
            array2[0, 1, 1, 2] = "им";
            array2[0, 1, 1, 3] = "их";
            array2[0, 1, 1, 4] = "ими";
            array2[0, 1, 1, 5] = "их";
            array2[0, 2, 0, 0] = "ие";
            array2[0, 2, 0, 1] = "их";
            array2[0, 2, 0, 2] = "им";
            array2[0, 2, 0, 3] = "их";
            array2[0, 2, 0, 4] = "ими";
            array2[0, 2, 0, 5] = "их";
            array2[0, 2, 1, 0] = "ие";
            array2[0, 2, 1, 1] = "их";
            array2[0, 2, 1, 2] = "им";
            array2[0, 2, 1, 3] = "их";
            array2[0, 2, 1, 4] = "ими";
            array2[0, 2, 1, 5] = "их";
            array2[1, 0, 0, 0] = "ая";
            array2[1, 0, 0, 1] = "ой";
            array2[1, 0, 0, 2] = "ой";
            array2[1, 0, 0, 3] = "ую";
            array2[1, 0, 0, 4] = "ой";
            array2[1, 0, 0, 5] = "ой";
            array2[1, 0, 1, 0] = "ые";
            array2[1, 0, 1, 1] = "ых";
            array2[1, 0, 1, 2] = "ым";
            array2[1, 0, 1, 3] = "ых";
            array2[1, 0, 1, 4] = "ыми";
            array2[1, 0, 1, 5] = "ых";
            array2[1, 1, 0, 0] = "яя";
            array2[1, 1, 0, 1] = "ей";
            array2[1, 1, 0, 2] = "ей";
            array2[1, 1, 0, 3] = "юю";
            array2[1, 1, 0, 4] = "ей";
            array2[1, 1, 0, 5] = "ей";
            array2[1, 1, 1, 0] = "ие";
            array2[1, 1, 1, 1] = "их";
            array2[1, 1, 1, 2] = "им";
            array2[1, 1, 1, 3] = "их";
            array2[1, 1, 1, 4] = "ими";
            array2[1, 1, 1, 5] = "их";
            array2[1, 2, 0, 0] = "";
            array2[1, 2, 0, 1] = "";
            array2[1, 2, 0, 2] = "";
            array2[1, 2, 0, 3] = "";
            array2[1, 2, 0, 4] = "";
            array2[1, 2, 0, 5] = "";
            array2[1, 2, 1, 0] = "";
            array2[1, 2, 1, 1] = "";
            array2[1, 2, 1, 2] = "";
            array2[1, 2, 1, 3] = "";
            array2[1, 2, 1, 4] = "";
            array2[1, 2, 1, 5] = "";
            array2[2, 0, 0, 0] = "ое";
            array2[2, 0, 0, 1] = "ого";
            array2[2, 0, 0, 2] = "ому";
            array2[2, 0, 0, 3] = "ое";
            array2[2, 0, 0, 4] = "ым";
            array2[2, 0, 0, 5] = "ом";
            array2[2, 0, 1, 0] = "ые";
            array2[2, 0, 1, 1] = "ых";
            array2[2, 0, 1, 2] = "ым";
            array2[2, 0, 1, 3] = "ых";
            array2[2, 0, 1, 4] = "ыми";
            array2[2, 0, 1, 5] = "ых";
            array2[2, 1, 0, 0] = "ее";
            array2[2, 1, 0, 1] = "его";
            array2[2, 1, 0, 2] = "ему";
            array2[2, 1, 0, 3] = "ее";
            array2[2, 1, 0, 4] = "им";
            array2[2, 1, 0, 5] = "ем";
            array2[2, 1, 1, 0] = "ие";
            array2[2, 1, 1, 1] = "их";
            array2[2, 1, 1, 2] = "им";
            array2[2, 1, 1, 3] = "их";
            array2[2, 1, 1, 4] = "ими";
            array2[2, 1, 1, 5] = "их";
            array2[2, 2, 0, 0] = "";
            array2[2, 2, 0, 1] = "";
            array2[2, 2, 0, 2] = "";
            array2[2, 2, 0, 3] = "";
            array2[2, 2, 0, 4] = "";
            array2[2, 2, 0, 5] = "";
            array2[2, 2, 1, 0] = "";
            array2[2, 2, 1, 1] = "";
            array2[2, 2, 1, 2] = "";
            array2[2, 2, 1, 3] = "";
            array2[2, 2, 1, 4] = "";
            array2[2, 2, 1, 5] = "";
            AdjectiveEndings = array2;
        }

        #region RusDeclension implementation

        /// <summary>
        /// Функция для склонения одного слова.
        /// Author: Железняков Юрий Юрьевич (aka SuperJur) (mailto:UGeleznyakov@esv.ryazan.ru)
        /// </summary>
        /// <param name="word">само слово</param>
        /// <param name="declCase">номер падежа </param>
        /// <param name="gender">пол</param>
        /// <param name="part">1-склонять как фамилию, 2-имя, 3-отчество</param>
        /// <returns></returns>
        private static string _declension(string word, int declCase /*=2*/, string gender /* = "*" */, int part /* =0 */)
        {
            if (word.Length == 2 && part == 1)
                return word;

            int z5 = InStr(word, "-");
            string z6 = string.Empty;

            if (z5 > 0) {
                string[] _name = word.Split('-');
                if (part == 1) {
                    z6 = "-" + _declension(_name[1], declCase, gender, part);
                    word = LCase(_name[0]);
                } else {
                    word = LCase(_name[0]) + "-" + UCase(Left(_name[1], 1)) + Mid(_name[1], 2);
                }
            } else {
                word = LCase(word);
            }

            string z7 = Right(word, 3);
            string z8 = Right(z7, 2);

            if (part == 1 && "иа их".Contains(z8)) {
                return word;
            }

            string z9 = Right(z8, 1);
            z5 = word.Length;

            // ая , ия, ел = орел, ок = брелок на = весна, ма = зима
            int za = InStr("ая ия ел ок яц ий па да ца ша ба та га ка на ма", z8);
            int zb = InStr("аеёийоуэюяжнгхкчшщ", Left(z7, 1));
            int zd = (za == 4) ? 5 : InStr("айяь", z9);

            DeclensionCase _dc = (DeclensionCase)Math.Max(declCase, -declCase);

            if (_dc == DeclensionCase.Nominative || z9 == "." ||
                (part == 2 && InStr("оиеу" + (gender == "ч" ? "" : "бвгджзклмнпрстфхцчшщъ"), z9) > 0) ||
                (part == 1 && InStr("мия мяэ лия кия жая лея", z7) > 0) ||
                (part < 3 && InStr("эы", z9) > 0)) {
                zd = 9;
            } else if (zd == 4 && gender == "ч") {
                zd = 2;
            } else if (part == 1) {
                if (InStr("оеиую", z9) + InStr("их ых аа еа ёа иа оа уа ыа эа юа яа", z8) > 0) {
                    zd = 9;
                } else if (gender != "ч") {
                    if (za == 1) {
                        zd = 7;
                    } else if (z9 == "а") {
                        zd = (za > 18 && z8 != "на") ? 1 : 6;
                    } else {
                        zd = 9;
                    }
                } else if (((InStr("ой ый", z8) > 0) && (z5 > 4) && (InStr("опой вбой", Right(word, 4)) == 0)) || (zb > 10 && za == 16)) {
                    zd = 8;
                }
            }

            string res;

            int ze = InStr("лец нёк вей бей дец пец мец нец рец вец бец тец жец аец иец ыец бер", z7);

            if (zd == 8 && _dc != DeclensionCase.Instrumental) {
                res = (zb > 15 || InStr("жий ний", z7) > 0) ? "е" : "о";
            } else if (word == "лев") {
                res = "ьв";
            } else {
                if (InStr("аеёийоуыэюя", Mid(word, z5 - 3, 1)) == 0 && (zb > 11 || zb == 0) && ze != 61) res = "";
                else if (za == 7) res = "л";
                else if (za == 10) res = "к";
                else if (za == 13) res = "йц";
                else if (ze == 0) res = "";
                else if (ze == 13) res = "е";
                else if (ze < 16) res = "ь" + (ze == 1 ? "ц" : (ze == 5 ? "к" : ""));
                else if (ze < 53) res = "ц";
                else if (ze < 65) res = "йц";
                else res = "р";
            }

            if (zd == 9 || ((part == 3) && (Right(word, 1) == "ы"))) {
                res = word;
            } else {
                StringBuilder temp = new StringBuilder(128);
                temp.Append("а у а ");
                if ((z8 == "ич" && !fomitch(word)) || (z8 == "ыш"))
                    temp.Append("е");
                else if ((z8 == "ов") || (z8 == "ев" && part == 1) || (z8 == "ин" && part != 2))
                    temp.Append("ы");
                else
                    temp.Append("о");

                temp.Append("ме ");
                temp.Append(InStr("гжкхш", Left(z8, 1)) > 0 ? "и" : "ы");
                temp.Append(" е у ");
                temp.Append(z8 == "ца" ? "е" : "о");
                temp.Append("йе я ю я ем");
                temp.Append(za == 16 ? "и" : "е");
                temp.Append(" и е ю ейе и и ь ьюи и и ю ейи ойойу ойойойойуюойойгомуго");
                temp.Append((res == "е") || (za == 16) || ((zb > 12) && (zb < 16)) ? "и" : "ы");
                temp.Append("мм");

                string template = temp.ToString();

                res = Left(word, z5 - ((zd > 6) || (!string.IsNullOrEmpty(res)) ? 2 : (zd > 0 ? 1 : 0))) + res +
                    RTrim(Mid(template, 10 * zd + 2 * (int)_dc - 3, 2));
            }

            return (string.IsNullOrEmpty(word) ? "" : (part > 0 ? UCase(Left(res, 1)) + ((declCase < 0) && (part > 1) ? "." : Mid(res, 2)) : res) + z6);
        }

        private static bool fomitch(string familia)
        {
            string[] fomitchi = new string[] { "ильич", "кузьмич", "фомич", "лукич" };

            foreach (string st in fomitchi) {
                if (st == familia) return true;
            }

            return false;
        }

        //_____________________________________________________________________________
        // z1 - фамилия имя отчество например Железняков Юрий Юрьевич
        // z2 - Падеж ( по  умолчанию = 2 - родительный)
        // 2 - родительный  ( нет кого?    ) Железнякова Юрия Юрьевича
        // 3 - дательный    ( кому?        ) Железнякову Юрию Юрьевичу
        // 4 - винительный  ( вижу кого?   ) Железнякова Юрия Юрьевича
        // 5 - творительный ( кем?         ) Железняковым Юрием Юрьевичем
        // 6 - предложный   ( о ком?       ) Железнякове Юрии Юрьевиче
        // Если задать Z2 меньше 0, то на выходе получим от -1=Железняков Ю. Ю. до -6=Железнякове Ю. Ю.
        // z3 - параметр Пол может не указываться, но при наличии фамилий с
        // инициалами точное определение пола невозможно, поэтому предлагается задавать пол этим
        // параметром  1 - мужской 2 - женский
        // ---------------------------------------------------------------------------------------
        // Бибик Галушка Цой Николайчик Наталия Петровна Герценберг Кривошей Капица-Метелица
        // Если Падеж(Фио ,1 ,3),       то на выходе получим Фамилия Имя Отчество и т.д.
        // Если Падеж(Фио ,1 ,3,"1"),  то                   Фамилия
        // Если Падеж(Фио ,1 ,3,"2"),  то                   Имя
        // Если Падеж(Фио ,1 ,3,"3"),  то                   Отчество
        // Если Падеж(Фио, 1 ,3,"12"), то                   Фамилия Имя
        // Если Падеж(Фио, 1 ,3,"23"), то                   Имя Отчество
        // Если Падеж(Фио,-1 ,3,"231"),то                   И. О. Фамилия
        // Если Падеж(Фио,-1 ,3,"23"), то                   И. О.

        /// <summary>
        /// 
        /// Author: Железняков Юрий Юрьевич (aka SuperJur) (mailto:UGeleznyakov@esv.ryazan.ru)
        /// </summary>
        /// <param name="fn">фамилия имя отчество, например Железняков Юрий Юрьевич</param>
        /// <param name="declCase">падеж</param>
        /// <param name="gender">параметр 'пол' может не указываться, но при наличии фамилий с
        /// инициалами точное определение пола невозможно, поэтому предлагается задавать пол этим
        /// параметром</param>
        /// <param name="part"></param>
        /// <param name="z5"></param>
        /// <returns></returns>
        public static string GetDeclension(string fn, DeclensionCase declCase, DeclensionGender gender = DeclensionGender.Neutral,
                                           string partsMask = "123", int part = 1)
        {
            try
            {
                string z6 = LCase(Right(RTrim(fn), 4));
                string z7 = Right(z6, 1);

                string sp = part.ToString();
                if (part < 4 && partsMask.Contains(sp)) {
                    string _FIO = Replace(Mid(fn, InStr(fn + " ", " ") + 1), ".", ". ").Trim();
                    string _base = ((part == 3) && (z7 == "ы") ? fn : Left(fn, InStr(fn + " ", " ") - 1));
                    string _token = _declension(_base, (int)declCase, Mid("ча" + z7, (gender == DeclensionGender.Neutral ? (z6 == "оглы" || z6 == "кызы" ? 1 : 3) : (int)gender), 1), part) + " ";
                    partsMask = Replace(partsMask, sp, _token);

                    return GetDeclension(_FIO, declCase, gender, partsMask, part + 1).Trim();
                } else {
                    return partsMask;
                }
            }
            catch
            {
                return "(ошибка)";
            }
        }

        #region Aux functions

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static string Replace(string p, string p_2, string p_3)
        {
            return p.Replace(p_2, p_3);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static int InStr(string s, string match)
        {
            return s.IndexOf(match) + 1;
        }

        private static int MinMax(int x, int min, int max)
        {
            //return Math.Max(Math.Min(x, max), min);
            return (x < min) ? min : (x > max) ? max : x;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static string Mid(string s, int i, int len)
        {
            int start = MinMax(i - 1, 0, s.Length);
            return s.Substring(start, MinMax(len, 0, s.Length - start));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static string Mid(string s, int i)
        {
            return (i > s.Length) ? string.Empty : s.Substring(i - 1);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static string Left(string s, int len)
        {
            return s.Substring(0, Math.Min(len, s.Length));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static string Right(string s, int len)
        {
            return s.Substring(s.Length - Math.Min(len, s.Length));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static string LCase(string s)
        {
            return s.ToLowerInvariant();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static string UCase(string p)
        {
            return p.ToUpperInvariant();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static string RTrim(string s)
        {
            return s.TrimEnd();
        }

        #endregion

        #endregion

        /// <summary>
        /// Выбирает правильную форму множественного числа для существительного
        /// </summary>
        /// <param name="num">Число</param>
        /// <param name="nounForms">3 формы существительного для чисел: 1, 2, 5</param>
        /// <returns></returns>
        public static string Pluralize(int num, string[] nounForms)
        {
            int lastDigit = num % 10;
            int last2Digits = num % 100;
            if (lastDigit == 1) {
                if (last2Digits != 11) {
                    return nounForms[0];
                }
            } else if (lastDigit >= 2 && lastDigit <= 4) {
                if (last2Digits < 10 || last2Digits > 20) {
                    return nounForms[1];
                }
            }
            return nounForms[2];
        }

        #region Spelling of numbers

        private static string[] hundrs = new string[] {
            "", "сто ", "двести ", "триста ", "четыреста ", "пятьсот ",
            "шестьсот ", "семьсот ", "восемьсот ", "девятьсот "
        };

        // 2..9
        private static string[] tens = new string[] {
            "двадцать ", "тридцать ", "сорок ", "пятьдесят ",
            "шестьдесят ", "семьдесят ", "восемьдесят ", "девяносто "
        };

        private static string[] ones = new string[] {
            "", "один ", "два ", "три ", "четыре ", "пять ", "шесть ",
            "семь ", "восемь ", "девять ", "десять ", "одиннадцать ",
            "двенадцать ", "тринадцать ", "четырнадцать ", "пятнадцать ",
            "шестнадцать ", "семнадцать ", "восемнадцать ", "девятнадцать "
        };

        // 0..1, 1..2
        private static string[][] onetwo = {
            new string[] {"один ", "два "},
            new string[] {"одна ", "две "}
        };

        // 0..3, 1..5
        private static string[][] abbrs = {
            new string[] {"миллиарда ", "миллиард ", "миллиардов ", "млрд. ", ""},
            new string[] {"миллиона ", "миллион ", "миллионов ", "млн. ", ""},
            new string[] {"тысячи ", "тысяча ", "тысяч ", "тыс. ", ""},
            new string[] {"", "", "", "", ""}
        };

        // Получить строчное написание числа
        public static string SpellNumber(int x)
        {
            string S, N;
            int i, j, z, x1, x2, x3;

            S = "";
            if (x < 0) {
                x = -x;
                S = "минус ";
            }

            N = x.ToString("000000000000");
            i = 1;

            /*while (N[i] == " ") {
                N[i] = '0';
                i++;
            }*/

            for (j = 0; j <= 3; j++) {
                ones[1] = onetwo[(j >= 2 ? 1 : 0)][0];
                ones[2] = onetwo[(j >= 2 ? 1 : 0)][1];

                z = j * 3; //  + 1
                x1 = (int)(N[z]) - 48;
                x2 = (int)(N[z + 1]) - 48;
                x3 = (int)(N[z + 2]) - 48;

                if (x1 + x2 + x3 == 0) {
                    if ((z == 10) && (N == "000000000000")) {
                        S = S + "ноль ";
                    }

                    i = 5;
                } else {
                    S = S + hundrs[x1];

                    if (x2 < 2) {
                        x3 += (10 * x2);
                    } else S = S + tens[x2 - 2];

                    S = S + ones[x3];

                    if ((x3 > 4) || (x3 == 0)) {
                        i = 3;
                    } else i = 1 + (x3 == 1 ? 1 : 0);
                }

                S = S + abbrs[j][i - 1];
            }

            S = S.Trim();

            return S;
        }

        #endregion

        #region Transliteration

        private static readonly string[][] TranslitTable = new string[][] {
            new string[] { "А",    "A",  "A",  "A",  "A",   "A" },
            new string[] { "Б",    "B",  "B",  "B",  "B",   "B" },
            new string[] { "В",    "V",  "V",  "V",  "V",   "V" },
            new string[] { "Г",    "G",  "G",  "G",  "G",   "G" },
            new string[] { "Д",    "D",  "D",  "D",  "D",   "D" },
            new string[] { "Е",    "E",  "E",  "E",  "E",   "E" },
            new string[] { "Ё",   "JO", "JO", "Y`", "JO",  "JO" },
            new string[] { "Ж",   "ZH", "ZH",  "J", "ZH",  "ZH" },
            new string[] { "З",    "Z",  "Z",  "Z",  "Z",   "Z" },
            new string[] { "И",    "I",  "I",  "I",  "I",   "I" },
            new string[] { "Й",    "J", "JI", "I`", "JJ",   "J" },
            new string[] { "К",    "K",  "K",  "K",  "K",   "K" },
            new string[] { "Л",    "L",  "L",  "L",  "L",   "L" },
            new string[] { "М",    "M",  "M",  "M",  "M",   "M" },
            new string[] { "Н",    "N",  "N",  "N",  "N",   "N" },
            new string[] { "О",    "O",  "O",  "O",  "O",   "O" },
            new string[] { "П",    "P",  "P",  "P",  "P",   "P" },
            new string[] { "Р",    "R",  "R",  "R",  "R",   "R" },
            new string[] { "С",    "S",  "S",  "S",  "S",   "S" },
            new string[] { "Т",    "T",  "T",  "T",  "T",   "T" },
            new string[] { "У",    "U",  "U",  "U",  "U",   "U" },
            new string[] { "Ф",    "F",  "F",  "F",  "F",   "F" },
            new string[] { "Х",   "KH", "KH",  "H", "KH",   "X" },
            new string[] { "Ц",    "C",  "C",  "C",  "C",   "C" },
            new string[] { "Ч",   "CH", "CH", "C`", "CH",  "CH" },
            new string[] { "Ш",   "SH", "SH", "S`", "SH",  "SH" },
            new string[] { "Щ", "SHCH",  "W", "H`", "HH", "SCH" },
            new string[] { "Ъ",   "\"",  "X", "X`", "``",   "`" },
            new string[] { "Ы",    "Y",  "Y",  "Y",  "Y",   "Y" },
            new string[] { "Ь",    "'",  "Q",  "X",  "`",   "'" },
            new string[] { "Э",   "EH", "EH", "E`", "EH",   "E" },
            new string[] { "Ю",   "JU", "JU", "U`", "JU",  "YU" },
            new string[] { "Я",   "JA", "JA", "A`", "JA",  "YA" }
        };

        private static void PrepareOrder(int[] order, int sch)
        {
            int tbLen = TranslitTable.Length;
            for (int i = 0; i < tbLen; i++) order[i] = i;

            for (int i = 0; i < tbLen; i++) {
                for (int k = i + 1; k < tbLen; k++) {
                    if (TranslitTable[k][sch].Length > TranslitTable[i][sch].Length) {
                        int t = order[i];
                        order[i] = order[k];
                        order[k] = t;
                    }
                }
            }
        }

        private static int FindSymbol(int[] order, int s, string check, int startIndex)
        {
            int idx = -1;
            for (int i = 0; i < TranslitTable.Length; i++) {
                int k = order[i];
                if (check.IndexOf(TranslitTable[k][s], startIndex, StringComparison.InvariantCultureIgnoreCase) == startIndex) {
                    idx = k;
                    break;
                }
            }
            return idx;
        }

        public static string Transliterate(TranslitScheme s, TranslitScheme t, string str)
        {
            string result = "";

            int[] order = new int[TranslitTable.Length];
            PrepareOrder(order, (int)s);

            int i = 0;
            while (i < str.Length) {
                bool isUpper = Char.IsUpper(str[i]);

                int dCnt;
                int idx = FindSymbol(order, (int)s, str, i);
                if (idx > -1) {
                    string[] row = TranslitTable[idx];
                    dCnt = row[(int)s].Length;
                    string tgt = row[(int)t];

                    bool nextLower = (i + 1 < str.Length) && Char.IsLower(str[i + 1]);

                    if (!isUpper) {
                        tgt = tgt.ToLower();
                    } else {
                        if (tgt.Length > 1 && nextLower) {
                            tgt = /*Char.ToUpper*/(tgt[0]) + tgt.Substring(1).ToLower();
                        } else {
                            //tgt = tgt.ToUpper();
                        }
                    }

                    result = result + tgt;
                } else {
                    dCnt = 1;
                    result = result + str[i];
                }
                i += dCnt;
            }

            return result;
        }

        #endregion
    }
}
