import { Component, ViewChild, OnInit, AfterViewInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Territory } from '../territory';
import { SourceVectorComponent, DrawInteractionComponent, MapComponent } from 'ngx-openlayers';
import * as ol from 'openlayers';
import { TerritoryService } from './territory.service';

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
  territory: Territory = {
    id: -1,
    userId: -1,
    name: '',
    instructions: '',
    boundary: [],
    created: new Date().toISOString(),
    updated: new Date().toISOString()
  };

  constructor(private route: ActivatedRoute, private territories: TerritoryService) { }

  ngOnInit() {
    const name = this.route.snapshot.paramMap.get('name');
    if (name === 'new') {
      this.isNew = true;
    } else {
      this.territories.getByName(name).subscribe((territory: Territory) => {
        this.territory = territory;
        this.polygon = new ol.geom.Polygon([ territory.boundary ]);
        this.vector.addFeature(new ol.Feature({ geometry: this.polygon }));
        this.map.getView().fit(this.polygon);
        this.setPoints();
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
    this.territories.save(this.territory).subscribe(
      (msg) => { console.log(msg); alert(msg); },
      (err) => { console.error(err); }
    );
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
        this.setUndoActive(false);
      }
    }
  }

  setUndoActive(value) {
    this.undoActive = value;
  }

  setPoints() {
    const polygon = this.polygon || new ol.geom.Polygon([[]]);
    this.territory.boundary = polygon.getCoordinates()[0];
    this.draw.setActive(this.territory.boundary.length === 0);
  }

}
