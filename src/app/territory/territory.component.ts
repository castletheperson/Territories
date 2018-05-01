import { Component, ViewChild, OnInit, AfterViewInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { HttpClient } from '@angular/common/http';
import { Territory } from '../territory';
import { SourceVectorComponent, DrawInteractionComponent, MapComponent } from 'ngx-openlayers';
import * as ol from 'openlayers';

@Component({
  selector: 'app-territory',
  templateUrl: './territory.component.html',
  styleUrls: ['./territory.component.css']
})
export class TerritoryComponent implements OnInit, AfterViewInit {

  @ViewChild(SourceVectorComponent)
  vectorComp: SourceVectorComponent;

  @ViewChild(DrawInteractionComponent)
  drawComp: DrawInteractionComponent;

  @ViewChild(MapComponent)
  mapComp: MapComponent;

  map: ol.Map;
  vector: ol.source.Vector;
  draw: ol.interaction.Draw;
  polygon: ol.geom.Polygon;

  isNew = false;
  undoActive = false;
  territory: Territory;

  constructor(private route: ActivatedRoute, private http: HttpClient) { }

  ngOnInit() {
    const name = this.route.snapshot.paramMap.get('name');
    if (name === 'new') {
      this.isNew = true;
      this.territory = {
        id: -1,
        userId: -1,
        name: '',
        instructions: '',
        points: [],
        created: new Date().toISOString(),
        updated: new Date().toISOString()
      };
    } else {
      this.http.get(`/api/territory/${name}`).subscribe((territory: Territory) => {
        this.territory = territory;
      });
    }
  }

  ngAfterViewInit() {
    this.map = this.mapComp.instance;
    this.draw = this.drawComp.instance;
    this.vector = this.vectorComp.instance;
    this.vector.on('change', () => this.setPoints());
    const modify = new ol.interaction.Modify({
      source: this.vector
    });
    this.map.addInteraction(modify);
  }

  onSave() {

  }

  setFeature(feature: ol.Feature) {
    this.polygon = feature.getGeometry() as ol.geom.Polygon;
  }

  deletePoints() {
    if (confirm('Do you really want to delete all the points?')) {
      this.vector.clear();
      this.polygon = null;
      this.setPoints();
      this.draw.setActive(true);
      this.setUndoActive(false);
    }
  }

  undoPoint() {
    if (this.undoActive) {
      this.draw.removeLastPoint();
      const coords = this.polygon.getCoordinates()[0];
      if (coords.length <= 2) {
        this.undoActive = false;
      }
    }
  }

  setUndoActive(value) {
    this.undoActive = value;
  }

  setPoints() {
    this.draw.setActive(false);
    if (this.polygon) {
      this.territory.points = this.polygon.getCoordinates()[0];
    } else {
      this.territory.points = [];
    }
  }

}
