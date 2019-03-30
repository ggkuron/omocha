{-# LANGUAGE QuasiQuotes #-}
module ColladaTestData (testData) where

import Text.RawString.QQ

testData :: String
testData = [r|<?xml version="1.0" encoding="utf-8"?>
<COLLADA xmlns="http://www.collada.org/2005/11/COLLADASchema" version="1.4.1">
  <asset>
    <contributor>
      <author>Blender User</author>
      <authoring_tool>Blender 2.78.0 commit date:2017-02-24, commit time:14:33, hash:e92f235283</authoring_tool>
    </contributor>
    <created>2019-01-31T13:37:28</created>
    <modified>2019-01-31T13:37:28</modified>
    <unit name="meter" meter="1"/>
    <up_axis>Z_UP</up_axis>
  </asset>
  <library_images/>
  <library_effects>
    <effect id="Material_007-effect">
      <profile_COMMON>
        <technique sid="common">
          <phong>
            <emission>
              <color sid="emission">0 0 0 1</color>
            </emission>
            <ambient>
              <color sid="ambient">0 0 0 1</color>
            </ambient>
            <diffuse>
              <color sid="diffuse">0.64 0.64 0.64 1</color>
            </diffuse>
            <specular>
              <color sid="specular">0.125 0.125 0.125 1</color>
            </specular>
            <shininess>
              <float sid="shininess">50</float>
            </shininess>
            <index_of_refraction>
              <float sid="index_of_refraction">1</float>
            </index_of_refraction>
          </phong>
        </technique>
      </profile_COMMON>
    </effect>
  </library_effects>
  <library_materials>
    <material id="Material_007-material" name="Material_007">
      <instance_effect url="#Material_007-effect"/>
    </material>
  </library_materials>
  <library_geometries>
    <geometry id="Cube_001-mesh" name="Cube.001">
      <mesh>
        <source id="Cube_001-mesh-positions">
          <float_array id="Cube_001-mesh-positions-array" count="48">3.556886 -4.590883 3.745798 3.556886 4.590883 3.745798 7.792813 4.171588 3.745798 7.792813 -4.171588 3.745798 7.792813 -4.171588 0.08200341 4.12902 4.171588 0.08200341 7.792813 4.171588 0.08200341 4.12902 4.171588 3.745798 4.12902 -4.171588 0.08200341 4.12902 -4.171588 3.745798 8.364949 4.590883 3.745798 8.364949 -4.590883 3.745798 4.719998 3.3 5.680753 7.279996 3.3 5.680753 4.719998 -3.3 5.680753 7.279996 -3.3 5.680753</float_array>
          <technique_common>
            <accessor source="#Cube_001-mesh-positions-array" count="16" stride="3">
              <param name="X" type="float"/>
              <param name="Y" type="float"/>
              <param name="Z" type="float"/>
            </accessor>
          </technique_common>
        </source>
        <source id="Cube_001-mesh-normals">
          <float_array id="Cube_001-mesh-normals-array" count="66">7.99155e-7 0 -1 0 -1 0 -7.99153e-7 0 -1 0 1 0 0 0 1 -0.578883 0.7898958 0.2023838 0.8722411 0 0.4890761 -0.8570746 0 0.5151924 0 0 -1 -1 0 0 0 -0.8318689 0.5549722 0 -0.8318689 -0.5549722 1 0 0 -1.8608e-6 0 -1 1.8608e-6 0 -1 0.5814829 0.7934409 0.1798028 0 0.9117702 0.410701 0 0.9117701 0.410701 0.8722413 0 0.489076 -0.8570747 0 0.5151921 0 -0.8318693 0.5549718 0 -0.8318693 -0.5549718</float_array>
          <technique_common>
            <accessor source="#Cube_001-mesh-normals-array" count="22" stride="3">
              <param name="X" type="float"/>
              <param name="Y" type="float"/>
              <param name="Z" type="float"/>
            </accessor>
          </technique_common>
        </source>
        <source id="Cube_001-mesh-map-0">
          <float_array id="Cube_001-mesh-map-0-array" count="192">0.391951 0.5286254 0.4382477 0.6062904 0.3941664 0.6062904 0.09781551 2.34422e-4 0.1949276 0.9997574 0.09781569 0.9997574 0.50005 0.6062894 0.4537535 0.5286244 0.4978346 0.5286244 2.34363e-4 2.34422e-4 0.09734684 0.9997572 2.34881e-4 0.9997572 0.3236273 0.8134965 0.3914823 0.5286254 0.3914823 0.8134965 0.2243161 0.5281502 0.1953963 2.34481e-4 0.2107198 2.49636e-4 0.7592213 0.5288967 0.717531 2.34363e-4 0.7660423 2.34363e-4 0.7102423 0.5290077 0.6685498 2.34481e-4 0.7170623 2.34363e-4 0.2925084 0.5286189 0.1953963 0.8227002 0.1953964 0.5286189 0.579449 2.34422e-4 0.6235307 0.9997655 0.5794507 0.9997655 0.5789803 2.34363e-4 0.48237 0.5281556 0.4515382 2.34955e-4 0.391951 0.6714745 0.5042325 0.6067594 0.519396 0.6714745 0.5042322 0.6719433 0.391951 0.7366585 0.4071143 0.6719435 0.4510694 2.34363e-4 0.3544648 0.5281567 0.3236273 2.36257e-4 0.6239994 2.34363e-4 0.6680795 0.9997655 0.6239995 0.9997655 0.391951 0.5286254 0.440463 0.5286254 0.4382477 0.6062904 0.09781551 2.34422e-4 0.1949276 2.34363e-4 0.1949276 0.9997574 0.50005 0.6062894 0.4515382 0.6062894 0.4537535 0.5286244 2.34363e-4 2.34422e-4 0.09734642 2.34363e-4 0.09734684 0.9997572 0.3236273 0.8134965 0.3236273 0.5286254 0.3914823 0.5286254 0.3078325 2.49044e-4 0.3231586 2.34363e-4 0.2921712 0.5281497 0.2921712 0.5281497 0.2243161 0.5281502 0.2107198 2.49636e-4 0.2107198 2.49636e-4 0.3078325 2.49044e-4 0.2921712 0.5281497 0.7592213 0.5288967 0.7243499 0.5288966 0.717531 2.34363e-4 0.7102423 0.5290077 0.6753713 0.5290079 0.6685498 2.34481e-4 0.2925084 0.5286189 0.2925084 0.8227002 0.1953963 0.8227002 0.579449 2.34422e-4 0.6235307 2.34363e-4 0.6235307 0.9997655 0.5789803 2.34363e-4 0.5502247 0.5281554 0.48237 0.5281556 0.391951 0.6714745 0.4071166 0.6067591 0.5042325 0.6067594 0.5042322 0.6719433 0.5193974 0.7366585 0.391951 0.7366585 0.4510694 2.34363e-4 0.4223195 0.5281558 0.3544648 0.5281567 0.6239994 2.34363e-4 0.6680811 2.34422e-4 0.6680795 0.9997655</float_array>
          <technique_common>
            <accessor source="#Cube_001-mesh-map-0-array" count="96" stride="2">
              <param name="S" type="float"/>
              <param name="T" type="float"/>
            </accessor>
          </technique_common>
        </source>
        <vertices id="Cube_001-mesh-vertices">
          <input semantic="POSITION" source="#Cube_001-mesh-positions"/>
        </vertices>
        <polylist material="Material_007-material" count="32">
          <input semantic="VERTEX" source="#Cube_001-mesh-vertices" offset="0"/>
          <input semantic="NORMAL" source="#Cube_001-mesh-normals" offset="1"/>
          <input semantic="TEXCOORD" source="#Cube_001-mesh-map-0" offset="2" set="0"/>
          <vcount>3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 </vcount>
          <p>0 0 0 7 0 1 9 0 2 8 1 3 3 1 4 9 1 5 10 2 6 3 2 7 2 2 8 6 3 9 7 3 10 2 3 11 12 4 12 15 4 13 13 4 14 13 5 15 10 5 16 2 5 17 13 6 18 11 6 19 10 6 20 14 7 21 1 7 22 0 7 23 6 8 24 8 8 25 5 8 26 5 9 27 9 9 28 7 9 29 11 10 30 14 10 31 0 10 32 1 4 33 2 4 34 10 4 35 3 8 36 0 8 37 9 8 38 10 11 39 12 11 40 1 11 41 4 12 42 2 12 43 3 12 44 0 13 45 1 13 46 7 13 47 8 1 48 4 1 49 3 1 50 10 14 51 11 14 52 3 14 53 6 3 54 5 3 55 7 3 56 12 4 57 14 4 58 15 4 59 7 15 60 1 15 61 12 15 62 12 16 63 13 16 64 2 16 65 2 17 66 7 17 67 12 17 68 13 18 69 15 18 70 11 18 71 14 19 72 12 19 73 1 19 74 6 8 75 4 8 76 8 8 77 5 9 78 8 9 79 9 9 80 11 20 81 15 20 82 14 20 83 1 4 84 7 4 85 2 4 86 3 8 87 11 8 88 0 8 89 10 21 90 13 21 91 12 21 92 4 12 93 6 12 94 2 12 95</p>
        </polylist>
      </mesh>
    </geometry>
  </library_geometries>
  <library_controllers/>
  <library_visual_scenes>
    <visual_scene id="Scene" name="Scene">
      <node id="Cube" name="Cube" type="NODE">
        <matrix sid="transform">1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1</matrix>
        <instance_geometry url="#Cube_001-mesh" name="Cube">
          <bind_material>
            <technique_common>
              <instance_material symbol="Material_007-material" target="#Material_007-material"/>
            </technique_common>
          </bind_material>
        </instance_geometry>
      </node>
    </visual_scene>
  </library_visual_scenes>
  <scene>
    <instance_visual_scene url="#Scene"/>
  </scene>
</COLLADA>|]