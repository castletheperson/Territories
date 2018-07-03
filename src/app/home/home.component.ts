import { Component, OnInit, ViewChild, AfterViewInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Territory } from '../territory';
import { MapComponent, SourceVectorComponent } from 'ngx-openlayers';
import { TerritoryService } from '../territory/territory.service';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit, AfterViewInit {

  @ViewChild(MapComponent)
  mapComp: MapComponent;

  @ViewChild(SourceVectorComponent)
  vectorComp: SourceVectorComponent;

  territories: Territory[] = [];

  constructor(private territoryService: TerritoryService) { }

  ngOnInit() {

  }

  ngAfterViewInit() {
    this.territoryService.getAll().subscribe((territories: Territory[]) => {
      this.territories = territories;
      this.zoomToFit();
    });
  }

  zoomToFit() {
    const map = this.mapComp.instance;
    const features = this.vectorComp.instance.getFeatures();
    console.log(features);
    if (features.length > 0) {
      const extent = features[0].getGeometry().getExtent();
      features.forEach(feature => ol.extent.extend(extent, feature.getGeometry().getExtent()));
      map.getView().fit(extent);
    }
  }

}
