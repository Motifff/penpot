;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.batch-toolbox
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.types.shape :as cts]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.texts :as dwt]
   [app.main.data.workspace.transforms :as dwtr]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*]]
   [app.main.ui.hooks :as hooks]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.shape-icon :as usi]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(declare shape-tree)
(declare extract-text-content)

(defn parse-csv
  "Simple CSV parser"
  [csv-text]
  (when (and csv-text (not (str/blank? csv-text)))
    (let [lines (str/split csv-text #"\r?\n")
          non-empty-lines (filter #(not (str/blank? %)) lines)]
      (mapv #(str/split % #",") non-empty-lines))))

(defn read-file-as-text
  "Read a file as text using FileReader"
  [file callback]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader)
          (fn [e]
            (let [result (.. e -target -result)]
              (callback result))))
    (.readAsText reader file)))

(mf/defc csv-display
  {::mf/wrap [mf/memo]}
  [{:keys [csv-data]}]
  (when csv-data
    [:div {:class (stl/css :csv-display)
           :style {:margin-top "16px"
                   :border "1px solid #ddd"
                   :border-radius "4px"
                   :overflow "auto"
                   :max-height "300px"}}
     [:table {:style {:width "100%"
                      :border-collapse "collapse"
                      :font-size "12px"}}
      [:tbody
       (for [[row-idx row] (d/enumerate csv-data)]
         [:tr {:key row-idx
               :style {:border-bottom (when (< row-idx (dec (count csv-data))) "1px solid #eee")}}
          (for [[col-idx cell] (d/enumerate row)]
            [:td {:key col-idx
                  :style {:padding "8px"
                          :border-right (when (< col-idx (dec (count row))) "1px solid #eee")
                          :vertical-align "top"}}
             cell])])]]]))

(mf/defc csv-file-selector
  {::mf/wrap [mf/memo]}
  [{:keys [on-csv-loaded]}]
  (let [file-input-ref (mf/use-ref)
        
        handle-file-select
        (mf/use-fn
         (fn [event]
           (when-let [file (-> event .-target .-files (aget 0))]
             (read-file-as-text file
                               (fn [text]
                                 (let [parsed-csv (parse-csv text)]
                                   (on-csv-loaded parsed-csv)))))))]
    
    [:div {:style {:margin-bottom "16px"}}
     [:input {:ref file-input-ref
              :type "file"
              :accept ".csv"
              :style {:display "none"}
              :on-change handle-file-select}]
     [:button {:class (stl/css :csv-button)
               :style {:padding "8px 16px"
                       :background-color "#007bff"
                       :color "white"
                       :border "none"
                       :border-radius "4px"
                       :cursor "pointer"
                       :font-size "14px"}
               :on-click #(when-let [input (mf/ref-val file-input-ref)]
                           (.click input))}
      "选择CSV文件"]]))

(defn find-text-shapes-in-board
  "递归查找Board中所有的文本图层"
  [objects board-id]
  (letfn [(collect-text-shapes [shape-id acc]
            (let [shape (get objects shape-id)]
              (cond
                (nil? shape) acc
                (= :text (:type shape)) (conj acc shape)
                (and (:shapes shape) (seq (:shapes shape)))
                (let [child-shapes (:shapes shape)]
                  (if (sequential? child-shapes)
                    (reduce (fn [acc child-id]
                              (collect-text-shapes child-id acc))
                            acc 
                            child-shapes)
                    acc))
                :else acc)))]
    (let [result (collect-text-shapes board-id [])]
      (if (sequential? result)
        result
        []))))

(defn match-csv-to-text-shapes
  "匹配CSV列名与文本内容"
  [csv-data text-shapes]
  (when (and csv-data (seq csv-data) (sequential? text-shapes) (seq text-shapes))
    (let [headers (first csv-data)
          data-rows (rest csv-data)]
      (when (and headers (seq headers))
        (doall
         (for [row data-rows]
           (when (and row (seq row))
             (let [row-data (zipmap headers row)]
               (doall
                (for [text-shape text-shapes]
                  (when text-shape
                    (let [current-text (extract-text-content text-shape)
                          layer-name (:name text-shape)]
                      (when (and current-text (not (str/blank? current-text)))
                        (loop [remaining-headers headers
                               best-match nil
                               best-score 0]
                          (if (empty? remaining-headers)
                            (when best-match
                              {:shape text-shape
                               :old-text current-text
                               :new-text (get row-data best-match)
                               :column best-match})
                            (let [header (first remaining-headers)]
                              (when (and header (not (str/blank? header)))
                                (let [score (cond
                                              (= (str/lower layer-name) (str/lower header)) 100
                                              (str/includes? (str/lower layer-name) (str/lower header)) (count header)
                                              (str/includes? (str/lower header) (str/lower layer-name)) (count layer-name)
                                              :else 0)]
                                  (if (> score best-score)
                                    (recur (rest remaining-headers) header score)
                                    (recur (rest remaining-headers) best-match best-score))))))))))))))))))))

(defn generate-board-name
  "根据CSV数据生成Board名称，格式：key1=value1 | key2=value2，包含所有字段"
  [csv-headers row-data]
  (let [;; 为所有字段创建key=value格式的名称部分
        name-parts (mapv (fn [field]
                          (let [value (get row-data field)]
                            (when (and value 
                                      (not (str/blank? value)))
                              (str field "=" value))))
                        csv-headers)
        ;; 过滤掉空值
        filtered-parts (filter identity name-parts)]
    (if (seq filtered-parts)
      (str/join " | " filtered-parts)
      "Generated Board")))

(defn parse-board-name
  "解析Board名称，提取key=value对，返回map"
  [board-name]
  (when (and board-name (str/includes? board-name "="))
    (let [parts (str/split board-name #" \\| ")
          key-value-pairs (mapv #(str/split % #"=" 2) parts)
          valid-pairs (filter #(= 2 (count %)) key-value-pairs)]
      (into {} (mapv (fn [[k v]] [k v]) valid-pairs)))))

(defn is-generated-board?
  "检查Board是否是通过批量生成创建的"
  [board-name]
  (and board-name 
       (or (str/includes? board-name "=")
           (= board-name "Generated Board"))))

(defn duplicate-board-with-text-replacement
  "复制Board并替换文本内容"
  ([objects board-id text-replacements]
   (duplicate-board-with-text-replacement objects board-id text-replacements nil nil nil))
  ([objects board-id text-replacements new-x new-y]
   (duplicate-board-with-text-replacement objects board-id text-replacements new-x new-y nil))
  ([objects board-id text-replacements new-x new-y custom-name]
  (let [board (get objects board-id)
        
        ;; 收集所有需要替换文本的shape ID映射
        text-replacement-map (reduce (fn [acc replacement]
                                      (when replacement
                                        (assoc acc (:id (:shape replacement)) (:new-text replacement))))
                                    {}
                                    text-replacements)]
    
    ;; 记录复制前的Board数量
    (let [before-objects (deref refs/workspace-page-objects)
          before-count (count (filter #(= :frame (:type (second %))) before-objects))]
      
      ;; 发送复制Board的事件
      (st/emit! (dws/duplicate-shapes [board-id]))
      
      ;; 使用延迟来确保复制完成后再替换文本
      (js/setTimeout 
       (fn []
         ;; 获取最新的objects来找到复制后的shapes
         (let [current-objects (deref refs/workspace-page-objects)
               original-board (get objects board-id)
               ;; 找到最新复制的顶层Board（与原始Board同级的frame）
               all-top-frames (filter (fn [[id shape]]
                                       (and (= :frame (:type shape))
                                            ;; 确保是顶层frame（没有parent或parent是根节点）
                                            (or (nil? (:parent-id shape))
                                                (= uuid/zero (:parent-id shape)))
                                            ;; 排除原始Board
                                            (not= id board-id)
                                            ;; 名称包含原始Board名称（复制的特征）
                                            (str/includes? (:name shape) (:name original-board))))
                                     current-objects)
               new-frame (when (seq all-top-frames)
                          ;; 找到最新添加的顶层frame
                          (let [sorted-frames (sort-by #(str (first %)) all-top-frames)]
                            (last sorted-frames)))]
           
           (js/console.log "🔍 找到" (count all-top-frames) "个候选顶层Board")
           (doseq [[id shape] all-top-frames]
             (js/console.log "📋 候选Board:" (:name shape) "Parent:" (:parent-id shape)))
           
           (when new-frame
             (let [[new-board-id new-board] new-frame]
               (js/console.log "🔧 选择处理的顶层Board:" (:name new-board) "Parent:" (:parent-id new-board))
               
               ;; 如果提供了自定义名称，先重命名Board
               (when custom-name
                 (js/console.log "📝 重命名Board为:" custom-name)
                 (st/emit! (dwsh/update-shapes [new-board-id]
                                              #(assoc % :name custom-name))))
               
               ;; 进行文本替换
               (let [new-text-shapes (find-text-shapes-in-board current-objects new-board-id)]
                (doseq [text-shape new-text-shapes]
                  (let [current-text (extract-text-content text-shape)
                        replacement (first (filter #(and %
                                                        (= current-text (:old-text %)))
                                                  text-replacements))]
                    (when (and replacement (:new-text replacement))
                      (let [new-content (txt/change-text (:content text-shape) (:new-text replacement))]
                        (st/emit! (dwsh/update-shapes [(:id text-shape)]
                                                     #(assoc % :content new-content))))))))
               
               ;; 文本替换完成后，再设置位置
               (when (and new-x new-y)
                 (js/console.log "📍 设置Board最终位置:" new-x new-y)
                 (js/setTimeout
                  (fn []
                    (st/emit! (dwtr/update-position new-board-id {:x new-x :y new-y} {:absolute? true})))
                  150)))))
       200))))))

(mf/defc batch-generator
  {::mf/wrap [mf/memo]}
  [{:keys [csv-data board objects]}]
  (let [columns-per-row (mf/use-state 3)    ;; 每行Board数量
        spacing (mf/use-state 50)           ;; Board间距
        handle-batch-generate
        (mf/use-fn
         (mf/deps csv-data board objects @columns-per-row @spacing)
         (fn []
           (js/console.log "🚀 开始批量生成Board...")
           
           (when (and csv-data board)
             (let [text-shapes (find-text-shapes-in-board objects (:id board))]
               (js/console.log "📝 找到" (count text-shapes) "个文本图层")
               
               (let [matches (match-csv-to-text-shapes csv-data text-shapes)
                     base-x (:x board)
                     base-y (:y board)
                     board-width (or (:width board) 300)
                     board-height (or (:height board) 200)
                     valid-rows (filter #(and % (seq (filter identity (flatten %)))) matches)]
                 (js/console.log "✨ 将生成" (count valid-rows) "个Board")
                 
                 ;; 为每个有效行单独处理，避免并发问题
                 (let [headers (first csv-data)
                       data-rows (rest csv-data)]
                   (loop [remaining-rows (map-indexed vector valid-rows)
                          remaining-data data-rows
                          delay-offset 0]
                     (when-let [[row-index row-matches] (first remaining-rows)]
                       (when row-matches
                         (let [valid-matches (filter identity (flatten row-matches))
                               grid-row (quot row-index @columns-per-row)
                               grid-col (mod row-index @columns-per-row)
                               new-x (+ base-x (* grid-col (+ board-width @spacing)))
                               new-y (+ base-y (* grid-row (+ board-height @spacing)))
                               ;; 生成基于当前行数据的Board名称
                               current-row-data (first remaining-data)
                               row-data-map (when current-row-data (zipmap headers current-row-data))
                               board-name (if row-data-map
                                           (generate-board-name headers row-data-map)
                                           (str "Board " (inc row-index)))]
                           (js/console.log "📍 Board" row-index ":" board-name "-> 位置(" new-x "," new-y ")")
                           (when (seq valid-matches)
                             ;; 使用递增的延迟来确保每个Board按顺序处理
                             (js/setTimeout
                              (fn []
                                (duplicate-board-with-text-replacement objects (:id board) valid-matches new-x new-y board-name))
                              (* delay-offset 300)))))
                       (recur (rest remaining-rows) (rest remaining-data) (inc delay-offset))))))))))]
    
    (when (and csv-data board)
      [:div {:style {:margin-top "16px"}}
       ;; 网格设置控件
       [:div {:style {:margin-bottom "12px"
                      :padding "12px"
                      :background-color "#f8f9fa"
                      :border-radius "4px"
                      :border "1px solid #e9ecef"}}
        [:div {:style {:font-weight "bold" :margin-bottom "8px"}} "网格布局设置"]
        
        [:div {:style {:display "flex" :gap "12px" :align-items "center" :margin-bottom "8px"}}
         [:label {:style {:min-width "80px"}} "每行数量:"]
         [:input {:type "number"
                  :min "1"
                  :max "10"
                  :value @columns-per-row
                  :style {:width "60px" :padding "4px" :border "1px solid #ccc" :border-radius "2px"}
                  :on-change #(reset! columns-per-row (js/parseInt (.. % -target -value)))}]]
        
        [:div {:style {:display "flex" :gap "12px" :align-items "center"}}
         [:label {:style {:min-width "80px"}} "间距:"]
         [:input {:type "number"
                  :min "0"
                  :max "500"
                  :value @spacing
                  :style {:width "80px" :padding "4px" :border "1px solid #ccc" :border-radius "2px"}
                  :on-change #(reset! spacing (js/parseInt (.. % -target -value)))}]
         [:span {:style {:font-size "12px" :color "#666"}} "px"]]]
       
       ;; 批量生成按钮
       [:button {:class (stl/css :batch-generate-button)
                 :style {:padding "12px 24px"
                         :background-color "#28a745"
                         :color "white"
                         :border "none"
                         :border-radius "4px"
                         :cursor "pointer"
                         :font-size "14px"
                         :font-weight "bold"
                         :width "100%"}
                 :on-click handle-batch-generate}
        (str "批量生成Board (每行" @columns-per-row "个)")]])))

(defn extract-text-content
  "Extract text content from a text shape"
  [shape]
  (when (= :text (:type shape))
    (when-let [content (:content shape)]
      (txt/content->text content))))

(mf/defc shape-item
  {::mf/wrap [mf/memo]}
  [{:keys [shape depth objects]}]
  (let [selected (mf/deref refs/selected-shapes)
        selected? (contains? selected (:id shape))
        has-children? (seq (:shapes shape))
        text-content (extract-text-content shape)]
    [:div
     [:div {:class (stl/css-case :layer-item true
                                :selected selected?)
            :style {:padding-left (str (* (inc depth) 16) "px")}}
      [:div {:class (stl/css :layer-name)}
       (:name shape)
       (when text-content
         [:div {:class (stl/css :text-content)
                :style {:color "#666"
                       :margin-top "4px"
                       :font-size "12px"
                       :white-space "pre-wrap"}}
          text-content])]]
     
     ;; Recursively render children
     (when has-children?
       [:& shape-tree {:shapes (into [] (map #(get objects %) (:shapes shape)))
                      :objects objects
                      :depth (inc depth)}])]))

(mf/defc shape-tree
  {::mf/wrap [mf/memo]}
  [{:keys [shapes objects depth]}]
  [:div
   (for [shape shapes]
     [:& shape-item {:shape shape
                    :key (:id shape)
                    :depth depth
                    :objects objects}])])

(mf/defc batch-tree
  {::mf/wrap [mf/memo]
   ::mf/wrap-props false}
  [{:keys [objects parent-size board-id]}]
  (let [board (get objects board-id)]
    [:div {:class (stl/css :element-list)}
     [:& shape-tree {:shapes [(get objects board-id)]
                    :objects objects
                    :depth 0}]]))

(mf/defc batch-toolbox*
  {::mf/wrap [mf/memo]}
  [{:keys [size-parent]}]
  (let [page     (mf/deref refs/workspace-page)
        objects  (:objects page)
        selected (mf/deref refs/selected-shapes)
        board    (when (= 1 (count selected))
                  (get objects (first selected)))
        
        csv-data* (mf/use-state nil)
        csv-data  (deref csv-data*)
        
        handle-csv-loaded
        (mf/use-fn
         (fn [data]
           (reset! csv-data* data)))]
    
    [:div#layers {:class (stl/css :layers)}
     [:div {:class (stl/css :tool-window-bar)}
      [:> title-bar* {:collapsable false
                     :title (if board
                             (:name board)
                             (tr "workspace.sidebar.batch.select-board"))}]]
     
     [:div {:class (stl/css :tool-window-content)
            :data-scroll-container true
            :style {:padding "16px"}}
      
      ;; CSV文件选择器
      [:& csv-file-selector {:on-csv-loaded handle-csv-loaded}]
      
      ;; CSV内容显示
      [:& csv-display {:csv-data csv-data}]
      
      ;; 批量生成按钮
      [:& batch-generator {:csv-data csv-data
                          :board board
                          :objects objects}]
      
      ;; Board层级结构（如果有选中的board）
      (when (and board (cfh/frame-shape? board))
        [:div {:style {:margin-top (if csv-data "32px" "16px")}}
         [:h4 {:style {:margin "0 0 16px 0"
                       :font-size "14px"
                       :font-weight "bold"}}
          (str "Board层级: " (:name board))]
         [:& batch-tree {:objects objects
                        :parent-size size-parent
                        :board-id (:id board)}]])]]))