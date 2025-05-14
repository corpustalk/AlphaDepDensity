from statistics import mean
import spacy
import re
import math
import pandas as pd
# from rapidfuzz.distance.LCSseq import distance
from spacy.symbols import ORTH
# from spacy.tokenizer import Tokenizer
from spacy.lang.char_classes import ALPHA, ALPHA_LOWER, ALPHA_UPPER
from spacy.lang.char_classes import CONCAT_QUOTES, LIST_ELLIPSES, LIST_ICONS
# from spacy.util import compile_prefix_regex, compile_suffix_regex
from spacy import displacy

class DependencyDensity:

    def __init__(self, text):
        self.text = text
        self.text = self.clean_text()
        self.nlp = spacy.load("en_core_web_sm")
        self.nlp_tokenize_rule()
        self.doc = self.nlp(self.text)

        # displacy.serve(self.doc, style='dep',
        #                options={'fine_grained': True, 'distance': 180},
        #                auto_select_port=True
        #                )  # http://localhost:5000/

        span_list = [token.i for token in self.doc if token.text.startswith("\'")]
        for span_idx in range(0, len(span_list), 1):
            if span_list[span_idx] != 0:
                with self.doc.retokenize() as retokenizer:
                    retokenizer.merge(self.doc[span_list[span_idx] - span_idx - 1:span_list[span_idx] - span_idx + 1])


    def nlp_tokenize_rule(self):

        infixes = (
                LIST_ELLIPSES
                + LIST_ICONS
                + [
                    r"(?<=[{al}])\.(?=[{au}])".format(al=ALPHA_LOWER, au=ALPHA_UPPER),  # 缩写词中的点（如 "U.S.A."）
                    r"(?<=[{a}])[,!?](?=[{a}])".format(a=ALPHA),  # 字母后的标点符号
                    r"(?<=[{a}])[:<>=](?=[{a}])".format(a=ALPHA),  # 字母后的符号
                ]
        )


        infixes = [x for x in infixes if "-" not in x]
        infixes = [x for x in infixes if "'" not in x]


        self.nlp.tokenizer.infix_finditer = spacy.util.compile_infix_regex(infixes).finditer

        self.nlp.tokenizer.add_special_case("cannot", [{ORTH: "cannot"}])

    def clean_text(self):
        text = self.text.strip()
        text = re.sub(r'\s+', ' ', text)

        # remove special characters
        text = re.sub(r"[^\w\s.,!?;'()-]", '', text)

        return text

    def calculate_word_distance(self,index_1,index_2,sent):
        distance = 0
        large_index = max(index_1,index_2)
        small_index = min(index_1,index_2)
        span = sent[small_index:large_index]
        for token in span:
            if ((token.pos_ == "PUNCT" or token.dep_ == 'punct')
                    and not re.search(r'[a-zA-Z]', token.text)):
                continue
            distance += 1

        return distance

    def calculate_word_position(self,i,sent):
        position = 0
        for token in sent[:i]:
            if ((token.pos_ == "PUNCT" or token.dep_ == 'punct')
                    and not re.search(r'[a-zA-Z]', token.text)):
                continue
            position += 1

        return position


    # acquire all the dependents of the token
    def children_dependencies(self, children_list, token_position,sent):
        dependency_distance = []
        dependency_postion = []
        for children in children_list:
            if children.i > token_position:
                if (children.pos_ != "PUNCT" and children.dep_ != 'punct'
                        and children.dep_ not in ['cc','conj']
                        and not "adv" in children.dep_
                        and not (children.dep_ == 'prep' and  children.head.tag_.startswith('V'))):
                    dependency_postion.append(self.calculate_word_position(children.i,sent))
                    dependency_distance.append(self.calculate_word_distance(children.i,token_position,sent))

        return {"dependency_position":dependency_postion,
                "dependency_distance":dependency_distance}

    # acquire all the governors of the token
    def head_dependencies(self, head, token_position,sent):
        if head.i > token_position:
            if head.pos_ != "PUNCT":
                position = self.calculate_word_position(head.i,sent)
                dependency_distance = self.calculate_word_distance(head.i,token_position,sent)
                return {"dependency_position":[position],
                        "dependency_distance":[dependency_distance]}

        return {"dependency_position":[],
                "dependency_distance":[]}

    # acquire all the dependents of the token
    def children_dependencies_of_complete_dependencies(self, children_list, token_position,sent):
        dependency_distance = []
        dependency_postion = []
        for children in children_list:
            if children.i < token_position:
                if children.pos_ != "PUNCT":
                        # and children.dep_ not in ['cc','conj']):
                        # and not "adv" in children.dep_
                        # and not (children.dep_ == 'prep' and  children.head.tag_.startswith('V'))):
                    dependency_postion.append(self.calculate_word_position(children.i,sent))
                    dependency_distance.append(self.calculate_word_distance(children.i,token_position,sent))

        return {"dependency_position":dependency_postion,
                "dependency_distance":dependency_distance}

    # acquire all the governors of the token
    def head_dependencies_of_complete_dependencies(self, head, token_position,token,sent):
        if head.i < token_position:
            if head.pos_ != "PUNCT" and token.pos_ != "PUNCT":
                position = self.calculate_word_position(head.i, sent)
                dependency_distance = self.calculate_word_distance(head.i, token_position, sent)
                return {"dependency_position":[position],
                        "dependency_distance":[dependency_distance]}

        return {"dependency_position":[],
                "dependency_distance":[]}

    def sentence_parsing(self,sent):
        tokens = []
        root_position = 1
        all_dependency_position = []
        all_dependency_distance = []
        all_dependency_position_complete = []
        all_dependency_distance_complete = []

        sent = self.nlp(sent.text)

        for token in sent:
            if (token.pos_ == "PUNCT" or token.dep_ == 'punct') and not re.search(r'[a-zA-Z]', token.text):
                continue
            if token.dep_ == "root":
                root_position = token.i
            tokens.append(token)
            # acquire all dependency relations establishing with the token
            # (the position of dependency should be after the token)
            dependent = self.children_dependencies(list(token.children), token.i,sent)
            governor = self.head_dependencies(token.head, token.i,sent)

            positions = dependent.get("dependency_position", [])
            positions.extend(governor.get("dependency_position", []))

            distances = (dependent.get("dependency_distance", []))
            distances.extend(governor.get("dependency_distance", []))

            all_dependency_position.append(positions)
            all_dependency_distance.append(distances)



            dependent_complete = self.children_dependencies_of_complete_dependencies(list(token.children), token.i,sent)
            governor_complete = self.head_dependencies_of_complete_dependencies(token.head, token.i,token,sent)
            positions_complete = dependent_complete.get("dependency_position", [])
            positions_complete.extend(governor_complete.get("dependency_position", []))
            distances_complete = (dependent_complete.get("dependency_distance", []))
            distances_complete.extend(governor_complete.get("dependency_distance", []))
            all_dependency_position_complete.append(positions_complete)
            all_dependency_distance_complete.append(distances_complete)


        return (tokens,root_position,all_dependency_position,all_dependency_distance,
                all_dependency_position_complete,all_dependency_distance_complete)


    def acquire_open_dependencies(self, sent):

        (tokens,root_position,all_dependency_position,all_dependency_distance,
         all_dependency_position_complete,all_dependency_distance_complete) = self.sentence_parsing(sent)
        token_list = []
        dependency_density = []
        alphadepdensity = []
        alphadepdensity_integrated = []
        inverse_dd = []
        all_open_dependency_list = []

        for id in range(0, len(tokens), 1):
            # if ((tokens[id].pos_ == "PUNCT" or tokens[id].dep_ == 'punct')
            #         and not re.search(r'[a-zA-Z]', tokens[id].text)):
            #     continue
            token_list.append(tokens[id])
            token_open_dependency = 0
            token_alphadepdensity = 0
            token_alphadepdensity_integrated = 0
            inverse_token_dd = 0
            all_open_dependency = []
            token_dependency_positions = all_dependency_position[:id + 1]
            token_dependency_distance = all_dependency_distance[:id + 1]
            for id_position in range(0, len(token_dependency_positions), 1):
                for id_dep in range(0,len(token_dependency_positions[id_position]),1):
                    position = token_dependency_positions[id_position][id_dep]
                    if position > id:
                        token_open_dependency += 1
                        token_alphadepdensity += token_dependency_distance[id_position][id_dep]
                        # if token_dependency_distance[id_position][id_dep] > 0:
                        #     inverse_token_dd += 1 / token_dependency_distance[id_position][id_dep]
                        # else:
                        #     inverse_token_dd += 0
                        all_open_dependency.append(token_dependency_distance[id_position][id_dep])

            dependency_density.append(token_open_dependency)
            alphadepdensity.append(token_alphadepdensity)
            inverse_dd.append(inverse_token_dd)
            all_open_dependency_list.append(all_open_dependency)




        for id in range(0, len(tokens), 1):
            if ((tokens[id].pos_ == "PUNCT" or tokens[id].dep_ == 'punct')
                    and not re.search(r'[a-zA-Z]', tokens[id].text)):
                continue
            alphadepdensity_integrated.append(sum(all_dependency_distance_complete[id]))

        return {"words": token_list,
                "root_position":root_position,
                "sentence_length":len(token_list),
                "dependency_density":dependency_density,
                "alphadepdensity":alphadepdensity,
                "completed_dependencies":alphadepdensity_integrated,
                "inverse_dd":inverse_dd,
                "all_open_dependency":all_open_dependency_list}

    def indices_of_dependency_density(self):

        mdd_list = []
        weighted_mdd_list = []
        normalized_weighted_mdd_list = []
        normalized_weighted_mdd_list_1 = []

        for sent in self.doc.sents:
            if not sent:
                continue
            results = self.acquire_open_dependencies(sent)
            try:
                mean_dependency_density = mean(results.get("dependency_density",[0]))
                mean_weighted_density = mean(results.get("alphadepdensity",[0]))
            except:
                mean_dependency_density,mean_weighted_density=0,0

            root_position = results.get("root_position")
            sentence_length = results.get("sentence_length")
            if mean_weighted_density*root_position*sentence_length == 0:
                normalized_weighted_mdd = 0
                normalized_weighted_mdd_1 = 0
            else:
                # normalized_weighted_mdd = abs(math.log(mean_weighted_density/math.sqrt(root_position*sentence_length)))
                normalized_weighted_mdd_1 = mean_weighted_density/math.sqrt(root_position*sentence_length)
                normalized_weighted_mdd_log = math.log(mean_weighted_density/math.sqrt(root_position*sentence_length))
                normalized_weighted_mdd = 1 / (1+math.exp(-normalized_weighted_mdd_log))
            mdd_list.append(mean_dependency_density)
            weighted_mdd_list.append(mean_weighted_density)
            normalized_weighted_mdd_list.append(normalized_weighted_mdd)
            normalized_weighted_mdd_list_1.append(normalized_weighted_mdd_1)

        return {
            "mdd":mean(mdd_list),
            "alphadepdensity":mean(weighted_mdd_list),
            "normalized_alphadepdensity":mean(normalized_weighted_mdd_list),
            "normalized_alphadepdensity_1":mean(normalized_weighted_mdd_list_1)
        }

    def indices_of_dependency_distance(self):

        def calculate_mean_dependency_distance(dep_labels, dep_positions):
            """
            Calculates the mean dependency distance of Chinese text.

            Args:
                dep_labels (list[str]): The list of dependency labels in the sentence.
                dep_positions (list[int]): The list of positions for each dependency label in the sentence.

            Returns:
                float: The mean dependency distance of the sentence.
            """
            # Initialize variables to store the dependency distances and their counts
            total_distance = 0
            count = 0

            # Iterate over the dependency labels and positions, and calculate the dependency distance for each one
            for i in range(len(dep_labels)):
                if dep_labels[i] == 'HED' or dep_labels[i] == 'WP':
                    continue
                else:
                    count += 1
                    # total_distance += abs(dep_positions[i] - dep_positions[dep_labels.index(dep_labels[i])])
                    total_distance += abs(dep_positions[i] - (i + 1))

            # Calculate the mean dependency distance
            if count:
                mean_distance = total_distance / count
            else:
                mean_distance = 0

            return mean_distance

        def calculate_normalized_mean_dependency_distance(mean_dependency_distance, dep_labels):
            """
            Calculates the normalized mean dependency distance of Chinese text.

            Args:
                dep_labels (list[str]): The list of dependency labels in the sentence.


            Returns:
                float: The normalized mean dependency distance of the sentence.
            """
            # Initialize variables to store the dependency distances and their counts

            root_position = dep_labels.index('ROOT') + 1

            # Iterate over the dependency labels and positions, and calculate the dependency distance for each one
            count = len([dep for dep in dep_labels if dep.lower() != 'punkt'])

            # Normalize the mean dependency distance
            # calculate the square root of the product of root position and count
            if count and mean_dependency_distance:
                normalized_mean_distance = abs(math.log(mean_dependency_distance / math.sqrt(root_position * count)))
            else:
                normalized_mean_distance = 0

            return normalized_mean_distance

        mdd_list, nmdd_list = [],[]
        for sent in self.doc.sents:
            dep_labels, dep_positions = [], []
            for token in sent:
                dep_labels.append(token.dep_)
                dep_positions.append(token.head.i)
            mean_dependency_distance = calculate_mean_dependency_distance(dep_labels, dep_positions)
            normalized_mean_dependency_distance = calculate_normalized_mean_dependency_distance(mean_dependency_distance, dep_labels)
            mdd_list.append(mean_dependency_distance)
            nmdd_list.append(normalized_mean_dependency_distance)
        return mean(mdd_list),mean(nmdd_list)

    def all_open_dependencies(self):


        results = {'words':[],'sentence_id':[],'dependency_density':[],'alphadepdensity':[]}
        i = 0
        for sent in self.doc.sents:
            # if not re.search(r'[a-zA-Z]', sent.text):
            #     continue
            result_dict = self.acquire_open_dependencies(sent)
            results['sentence_id'].extend([i]*len(result_dict['dependency_density']))
            results['dependency_density'].extend(result_dict['dependency_density'])
            results['alphadepdensity'].extend(result_dict['alphadepdensity'])
            results['words'].extend(result_dict['words'])
            i += 1

        return results



if __name__ == '__main__':
    text = "Some of the drops that were the most fortunate, including Aqua, fell on a mountainside."
    dependency_density = DependencyDensity(text)

    print(dependency_density.indices_of_dependency_density())
    dependencies = dependency_density.all_open_dependencies()
    print(dependencies)







